"""Add native print definition unsupported by the artifact API. No workbook library authoring."""
from pathlib import Path
from zipfile import ZipFile, ZIP_DEFLATED
import sys
from xml.etree import ElementTree as ET
p = Path(sys.argv[1])
ns = 'http://schemas.openxmlformats.org/spreadsheetml/2006/main'
ET.register_namespace('', ns)
with ZipFile(p) as z:
    parts = {n: z.read(n) for n in z.namelist()}
r = ET.fromstring(parts['xl/workbook.xml'])
names = r.find(f'{{{ns}}}definedNames')
if names is None:
    names = ET.SubElement(r, f'{{{ns}}}definedNames')
ET.SubElement(names, f'{{{ns}}}definedName', {'name':'_xlnm.Print_Area','localSheetId':'0'}).text="'換気計算(一般）'!$C$3:$V$25"
parts['xl/workbook.xml'] = ET.tostring(r, encoding='utf-8', xml_declaration=True)
with ZipFile(p, 'w', ZIP_DEFLATED) as z:
    for n, b in parts.items(): z.writestr(n, b)
print('PASS native print area C3:V25')
