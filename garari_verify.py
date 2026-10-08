"""Static integrity check; --core also runs pure tests in an isolated AutoCAD core."""
from pathlib import Path
import csv
import sys
import tempfile
import subprocess
import shutil
sys.stdout.reconfigure(encoding='utf-8', errors='replace')

ROOT = Path(__file__).resolve().parent
for name in ('garari_table.lsp', 'GARARITABLE_tests.lsp'):
    text = (ROOT / name).read_text(encoding='utf-8-sig')
    depth = 0
    quoted = escaped = comment = False
    for ch in text:
        if comment:
            if ch == '\n':
                comment = False
            continue
        if quoted:
            if escaped:
                escaped = False
            elif ch == '\\':
                escaped = True
            elif ch == '"':
                quoted = False
            continue
        if ch == ';':
            comment = True
        elif ch == '"':
            quoted = True
        elif ch == '(':
            depth += 1
        elif ch == ')':
            depth -= 1
            assert depth >= 0, f'{name}: extra close parenthesis'
    assert depth == 0 and not quoted, f'{name}: unbalanced form ({depth})'
    print(f'{name}: balanced forms')
for name, keys in (
    ('garari_master.csv', ['SHAPE', 'DIA', 'FD', 'MATERIAL', 'MESH', 'MESH_SPEC', 'FD_TEMP', 'SOUND']),
    ('room_master.csv', ['MODEL']),
):
    path = ROOT / name
    assert path.read_bytes().startswith(b'\xef\xbb\xbf'), f'{name}: missing UTF-8 BOM'
    with path.open(encoding='utf-8-sig', newline='') as stream:
        rows = list(csv.DictReader(stream))
    seen = set()
    for row in rows:
        assert None not in row and None not in row.values(), f'{name}: wrong field count'
        key = tuple(row[k] for k in keys)
        assert key not in seen, f'{name}: duplicate key {key}'
        seen.add(key)
        assert row['APPROVED'] in ('0', '1')
        assert row['MODEL'] and row['DIA'].isdigit() and int(row['DIA']) > 0
        assert not any('\n' in v or '\r' in v for v in row.values())
    print(f'{name}: {len(rows)} unique valid rows / UTF-8 BOM')
if '--core' in sys.argv:
    core = Path(r'C:\Program Files\Autodesk\AutoCAD 2026\accoreconsole.exe')
    with tempfile.TemporaryDirectory(prefix='garari-check-', ignore_cleanup_errors=True) as tmp:
        directory = Path(tmp)
        for name in ('garari_table.lsp', 'GARARITABLE_tests.lsp', 'garari_master.csv', 'room_master.csv'):
            shutil.copy2(ROOT / name, directory / name)
        base = directory.as_posix()
        script = directory / 'test.scr'
        script.write_text(
            '(princ (strcat "LISPSYS=" (itoa (getvar "LISPSYS"))))\n'
            f'(setq garari:test-trusted (getvar "TRUSTEDPATHS"))\n'
            f'(setvar "TRUSTEDPATHS" "{base}")\n'
            f'(load "{base}/garari_table.lsp")\n'
            f'(setq garari:directory "{base}")\n'
            f'(load "{base}/GARARITABLE_tests.lsp")\n'
            'GARARITABLETEST\nGARARITABLECOMTEST\n(setvar "TRUSTEDPATHS" garari:test-trusted)\n_QUIT\n_Y\n', encoding='ascii')
        result = subprocess.run([str(core), '/s', str(script), '/l', 'en-US',
                                 '/isolate', 'GARARI_TEST', str(directory / 'profile')],
                                cwd=directory, capture_output=True, timeout=90)
        output = result.stdout.decode('utf-16-le', errors='replace')
        (ROOT / 'GARARITABLE_core_test.log').write_text(output, encoding='utf-8')
        print(output[-7000:])
        assert 'GARARITABLETEST PASS=' in output, 'Core tests did not pass (see log)'
        assert 'GARARITABLECOMTEST PASS' in output, 'Excel COM test did not pass (see log)'
        from openpyxl import load_workbook
        artifact = directory / '日本語 試験' / 'acceptance.xlsx'
        book = load_workbook(artifact)
        assert set(book.sheetnames) == {'機器表（ガラリ）', 'CAD_DATA', 'エラー一覧', '選定設定'}
        table = book['機器表（ガラリ）']
        assert table['A2'].value == '01' and table['A2'].data_type == 's'
        assert (table['E2'].value, table['F2'].value, table['G2'].value) == (2, 1, 3)
        assert table['G2'].data_type == 'n' and table['G3'].value is None
        assert table['C3'].value == '屋外：P-18VSQD4'
        assert table['C7'].value == '室内：P-13WQU'
        assert 'A2:A3' in {str(x) for x in table.merged_cells.ranges}
        assert not any(x.min_col == 3 for x in table.merged_cells.ranges)
        assert str(table.page_setup.paperSize) == str(table.PAPERSIZE_A3)
        assert table.page_setup.orientation == 'landscape'
        assert not any(c.data_type == 'f' for sheet in book for row in sheet for c in row)
        assert book['CAD_DATA'].max_row == 6
        print('Excel artifact verification: PASS (4 sheets, literal text, numeric quantities, merges, A3)')
        shutil.copy2(artifact, ROOT / 'GARARITABLE_acceptance.xlsx')
