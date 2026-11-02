"""Independent read-only verification of Excel's saved output and source-copy safety."""
from pathlib import Path
import hashlib
import sys
import openpyxl

root = Path(__file__).resolve().parent
test = Path(sys.argv[1])
reference = Path(sys.argv[2])
count = 0
def check(condition, label):
    global count
    assert condition, label
    count += 1
    print('PASS:', label)

def balanced(path):
    depth = 0
    quoted = escaped = False
    for row in path.read_text(encoding='utf-8-sig').splitlines():
        for c in row:
            if quoted:
                if escaped: escaped = False
                elif c == '\\': escaped = True
                elif c == '"': quoted = False
            elif c == ';': break
            elif c == '"': quoted = True
            elif c == '(': depth += 1
            elif c == ')':
                depth -= 1
                assert depth >= 0, str(path)
    return depth == 0 and not quoted

for name in ['07_kikitable.lsp', '06_kikiset.lsp', 'kanki_calc_regression.lsp',
             'kanki_failure_regression.lsp', 'kanki_attribute_regression.lsp']:
    check(balanced(root/name), 'balanced LISP: '+name)
check(hashlib.sha256(reference.read_bytes()).digest() == hashlib.sha256((test/'failure.xlsx').read_bytes()).digest(),
      'all failure cases left the source copy byte-identical')
log = (test/'console.log').read_text(encoding='utf-16-le')
check('FAIL:' not in log and 'REGRESSION_ERROR:' not in log, 'no failed assertions or uncaught errors')
check('KANKI_REGRESSION_PASS=' in log and 'KANKI_FAILURE_PASS_TOTAL=' in log, 'both AutoCAD suites completed')
wb = openpyxl.load_workbook(test/'fixture.xlsx', data_only=False)
cached = openpyxl.load_workbook(test/'fixture.xlsx', data_only=True)
original = openpyxl.load_workbook(reference, data_only=False)
s = wb['換気計算(一般）']; v = cached[s.title]
check(s['D6'].value == '事務室', 'saved room')
check(s['E6'].value == 43.7 and s['F6'].value == 2.4, 'saved CAD dimensions')
check(abs(v['G6'].value - 104.88) < 1e-9, 'saved Excel-calculated volume')
check(v['R6'].value == 430, 'saved single-fan airflow after shrink')
check(s['V3'].value.replace('\n','') == '送風機形式' and s['V6'].value == '低騒音形', 'standard V column still outputs CAD TYPE')
check(v['T6'].value == '要設定', 'reintroduced room does not inherit deleted settings')
check(s['G6'].value.startswith('=IF(AND('), 'managed formula is a formula')
check(all(s.cell(r,c).value is None for r in range(7,36) for c in range(3,23)), 'no stale managed detail')
check(s['A6'].value == 'LEFT-KEEP' and s['C36'].value == 'BOTTOM-KEEP', 'left and lower data retained')
check(s['W36'].value == '=SUM(1,2)' and v['W36'].value == 3, 'right formula preserved and recalculated')
check(str(s.print_area).endswith('$C$3:$V$35'), 'expanded native print area')
check(s.row_dimensions[35].height == s.row_dimensions[25].height, 'inserted row height retained')
check(set(str(x) for x in s.merged_cells) == set(str(x) for x in openpyxl.load_workbook(root/'換気計算テンプレート.xlsx')[s.title].merged_cells), 'header merges retained')
def cells(sheet):
    return {(c.coordinate,c.data_type):c.value for row in sheet for c in row if c.value is not None}
check(cells(wb['CAD_DATA']) == cells(original['CAD_DATA']), 'aircon raw sheet unchanged by ventilation')
raw = wb['CAD_DATA_KANKI']
headers = [c.value for c in raw[1]]
record = dict(zip(headers, [c.value for c in raw[2]]))
check(record['FLOOR_AREA'] == '43.7' and record['CEILING_HEIGHT'] == '2.4', 'raw attribute strings retained')
check(record['FLOOR'] == '01' and record['HANDLE'] == 'A1', 'raw floor and handle retained')
check(sum(any(c.value is not None for c in row) for row in raw) == 2, 'one raw row per current block')
check('KANKI_旧設定' in wb.sheetnames and wb['KANKI_旧設定'].max_row > 30, 'removed settings archived')
check(all(c.data_type != 'e' for row in v.iter_rows(min_row=6,max_row=35,min_col=3,max_col=22) for c in row), 'no saved errors in generated calculation range')
template = openpyxl.load_workbook(root/'換気計算テンプレート.xlsx')
t = template[s.title]
check(str(t.print_area).endswith('$C$3:$V$25'), 'template detail boundary')
check(all(t.cell(r,c).value is None for r in range(6,26) for c in range(3,23)), 'template contains no stale data or copied broken formulas')
check(all(c.data_type != 'e' for row in t for c in row), 'template contains no cell errors')
print('KANKI_PYTHON_PASS='+str(count))
