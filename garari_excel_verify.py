"""Read-only acceptance checks against disposable Excel COM regression fixtures."""
from pathlib import Path
from copy import copy
import os
import sys
from openpyxl import load_workbook
root=Path(sys.argv[1]) if len(sys.argv)>1 else Path(os.environ['TEMP'])/'garari-acceptance-20261012'
for filename, origin in [('fixture.xlsx',(1,1)),('offset.xlsx',(5,2))]:
    b=load_workbook(root/filename,data_only=False)
    assert b['空調機器表']['A1'].value=='KEEP'
    assert b['空調機器表']['B1'].value=='=SUM(2,3)'
    assert b['CAD_DATA']['A1'].value=='KEEP RAW'
    table=b['機器表（ガラリ）'];r,c=origin
    assert table.cell(r,c).value=='記号'
    assert table.cell(r+1,c).value=='01'
    assert [table.cell(r+1,c+i).value for i in range(4,9)]==[1,0,0,0,1]
    assert table.cell(r+3,c+2).value=='室内：P-18WQU'
    assert b['GARARI_CAD_DATA']['L2'].value=='P-18WQU'
    assert b['GARARI_CAD_DATA']['A3'].value is None
    assert {k for k in b.defined_names} >= {'_GARARI_TABLE','_GARARI_DATA','_GARARI_ERRORS','_GARARI_SETUP'}
    assert all('#N/A' not in b.defined_names[k].attr_text for k in b.defined_names)
    if filename=='offset.xlsx':
        candidates=sorted(root.glob('offset_GARARI_backup-*.xlsx'),key=lambda p:p.stat().st_mtime)
        baseline=load_workbook(candidates[-1])['機器表（ガラリ）']
        assert table['A1'].value=='TEMPLATE TITLE'
        assert table['M5'].value=='=2+6'
        assert table['B20'].value is None
        assert table.column_dimensions['B'].width==baseline.column_dimensions['B'].width
        assert table.row_dimensions[5].height==baseline.row_dimensions[5].height
        assert copy(table['B5'].fill)==copy(baseline['B5'].fill)
    else:
        assert table.page_setup.orientation=='landscape'
        assert int(table.page_setup.paperSize)==8
    b.close()
    print(f'{filename}: quantities replaced; unrelated formulas/data and template retained')
locked=load_workbook(root/'locked.xlsx')
assert locked['空調機器表']['A1'].value=='KEEP'
assert '機器表（ガラリ）' not in locked.sheetnames
locked.close()
assert not list(root.glob('locked_GARARI_backup-*.xlsx'))
print('readonly fixture: no GARARI sheet written')
for filename in ('backupfail.xlsx','busy.xlsx','ownerbusy.xlsx'):
    b=load_workbook(root/filename)
    assert '機器表（ガラリ）' not in b.sheetnames
    assert b['空調機器表']['B1'].value=='=SUM(2,3)'
    b.close()
    print(f'{filename}: failure did not write any GARARI sheet')
