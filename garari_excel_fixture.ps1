param([string]$TestDirectory)
$garariExcel = New-Object -ComObject Excel.Application
$garariExcel.DisplayAlerts = $false
try {
    foreach ($garariName in @('fixture.xlsx','offset.xlsx','locked.xlsx','busy.xlsx','backupfail.xlsx','ownerbusy.xlsx')) {
        $garariBook = $garariExcel.Workbooks.Add()
        $garariSheet = $garariBook.Worksheets.Item(1)
        $garariSheet.Name = '空調機器表'
        $garariSheet.Range('A1').Value2 = 'KEEP'
        $garariSheet.Range('B1').Formula = '=SUM(2,3)'
        $garariOther = $garariBook.Worksheets.Add()
        $garariOther.Name = 'CAD_DATA'
        $garariOther.Range('A1').Value2 = 'KEEP RAW'
        if ($garariName -eq 'offset.xlsx') {
            $garariTable = $garariBook.Worksheets.Add()
            $garariTable.Name = '機器表（ガラリ）'
            $garariTable.Range('A1').Value2 = 'TEMPLATE TITLE'
            $garariTable.Range('M5').Formula = '=2+6'
            $garariTable.Range('B5:K30').ColumnWidth = 23
            $garariTable.Range('B5:K30').RowHeight = 31
            $garariTable.Range('B5:K30').Interior.Color = 14277081
            $garariTable.Range('B20').Value2 = 'STALE'
            $garariBook.Names.Add('_GARARI_TABLE',"='機器表（ガラリ）'!`$B`$5:`$K`$30") | Out-Null
        }
        $garariBook.SaveAs((Join-Path $TestDirectory $garariName),51)
        $garariBook.Close($false)
    }
} finally {
    $garariExcel.Quit()
    [Runtime.InteropServices.Marshal]::FinalReleaseComObject($garariExcel) | Out-Null
}
