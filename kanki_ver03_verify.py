"""Read-only independent checks against the user's ver03 and native Excel output."""
from pathlib import Path
import hashlib
import re
import sys
import openpyxl
from openpyxl.formula.translate import Translator

source, output = map(Path, sys.argv[1:3])
src = openpyxl.load_workbook(source, data_only=False)
out = openpyxl.load_workbook(output, data_only=False)
values = openpyxl.load_workbook(output, data_only=True)
name = '換気計算(一般）'
s, t, v = src[name], out[name], values[name]
count = 0
def check(ok, label):
    global count
    assert ok, label
    count += 1
    print('PASS:', label)

check(t['R3'].value == s['R3'].value == '必要換気能力', 'actual R3 retained')
check(t['V3'].value == s['V3'].value == '備考', 'V remains remarks')
check([[c.value for c in row] for row in s.iter_rows(min_row=1,max_row=5,max_col=22)] ==
      [[c.value for c in row] for row in t.iter_rows(min_row=1,max_row=5,max_col=22)], 'all original headers retained')
check(set(map(str,s.merged_cells)) == set(map(str,t.merged_cells)), 'original header merges retained')
check(abs(s.column_dimensions['V'].width-t.column_dimensions['V'].width)<0.15, 'V column width retained')
rooms = {t.cell(r,4).value:r for r in range(6,14)}
check(len(rooms) == 8 and None not in rooms, 'eight current rooms saved')
check('院長室' not in rooms, 'floor-missing block not inferred into a room')
check('ロッカールーム' not in rooms, 'removed room absent')
check(t.cell(rooms['患者用ラバトリー'],22).value == '強設定', 'original manual remark retained')
check(t.cell(rooms['機械室'],22).value == '機器発熱より', 'other manual remark retained')
check(v.cell(rooms['消毒室'],8).value == 2 and v.cell(rooms['消毒室'],17).value == 3, 'manual people and method retained')
check(all(v.cell(r,20).value != '○' for r in range(6,14)), 'unconfigured rooms never marked sufficient')
check(all(c.data_type != 'e' for row in v.iter_rows(min_row=6,max_row=37,min_col=3,max_col=22) for c in row), 'no errors in generated range')
check(all(t.cell(r,c).value is None for r in range(14,38) for c in range(3,23)), 'no stale room rows or remarks')
check(str(t.print_area).endswith('$C$3:$V$37'), 'expanded print boundary retained after shrink')
check(t['C38'].value == s['C16'].value == '1F', 'lower placeholders preserved')
check(t['B38'].value == Translator(s['B16'].value,origin='B16').translate_formula('B38'), 'lower helper formula follows insertion')
archive = out['KANKI_旧設定']
check(any(row[1] == '1F / ロッカールーム' and '手入力備考を保持' in row for row in archive.iter_rows(min_row=2,values_only=True)), 'deleted room remark recoverable')
raw = out['CAD_DATA_KANKI']
check(sum(any(c.value is not None for c in row) for row in raw)==10, 'nine blocks in raw including missing-floor block')
headers=[c.value for c in raw[1]]
check(all(k in headers for k in ['FLOOR_AREA','CEILING_HEIGHT','TYPE','HANDLE']), 'room attributes and equipment type in raw')
check(any(row[headers.index('LOCATION')] == '院長室' for row in raw.iter_rows(min_row=2,values_only=True)), 'excluded room remains in raw')
check(all(t.cell(r,22).value not in ['機器形式テスト','天井カセット形マイコンタイプ'] for r in range(6,14)), 'TYPE does not overwrite remarks')

# Every unowned sheet must keep its values/formulas, allowing native reference
# changes caused by the explicitly inserted rows on the two owned sheets.
def print_end(sheet):
    return max(int(x) for x in re.findall(r'\$[A-Z]+\$(\d+)',str(sheet.print_area)))
shifts=[(name,15,22),('機器表 (換気設備)',25,print_end(out['機器表 (換気設備)'])-25)]
def expected_formula(value):
    if not isinstance(value,str) or not value.startswith('='): return value
    for sheet,end,delta in shifts:
        pattern=r"('"+re.escape(sheet)+r"'!)(\$?[A-Z]{1,3})(\$?)(\d+)"
        value=re.sub(pattern,lambda m:m[1]+m[2]+m[3]+str(int(m[4])+delta if int(m[4])>end else int(m[4])),value)
    return value
def cells(sheet):
    return {c.coordinate:c.value for row in sheet for c in row if c.value is not None}
for sheet in src:
    if sheet.title in [name,'CAD_DATA_KANKI','機器表 (換気設備)']: continue
    expected={key:expected_formula(value) for key,value in cells(sheet).items()}
    actual=cells(out[sheet.title])
    check(expected==actual, 'unowned sheet values/formulas preserved: '+sheet.title)
    check(len(sheet._charts)==len(out[sheet.title]._charts), 'native chart count retained: '+sheet.title)
check(hashlib.sha256(source.read_bytes()).digest()==hashlib.sha256(output.with_name('failure.xlsx').read_bytes()).digest(), 'reference copy byte-identical')
print('VER03_PYTHON_PASS='+str(count))
