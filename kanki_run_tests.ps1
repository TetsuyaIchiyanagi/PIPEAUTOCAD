param([string]$Reference, [string]$TestDirectory, [switch]$ChartOnly, [switch]$HeaderOnly, [switch]$Ver03Only)
$ErrorActionPreference='Stop'
New-Item -ItemType Directory -Path $TestDirectory -Force | Out-Null
$kankiCode=Join-Path $TestDirectory 'code'
New-Item -ItemType Directory -Path $kankiCode -Force | Out-Null
foreach($kankiFile in @('07_kikitable.lsp','06_kikiset.lsp','kanki_calc_regression.lsp','kanki_failure_regression.lsp','kanki_attribute_regression.lsp','kanki_chart_regression.lsp','kanki_header_regression.lsp','kanki_ver03_regression.lsp','換気計算テンプレート.xlsx')) {
 Copy-Item -LiteralPath (Join-Path $PSScriptRoot $kankiFile) -Destination $kankiCode -Force
}
$kankiRoot=$kankiCode.Replace('\','/')
Copy-Item -LiteralPath $Reference -Destination (Join-Path $TestDirectory 'fixture.xlsx') -Force
Copy-Item -LiteralPath $Reference -Destination (Join-Path $TestDirectory 'failure.xlsx') -Force
$kankiOriginalHash=(Get-FileHash -LiteralPath (Join-Path $TestDirectory 'failure.xlsx')).Hash
$kankiDir=$TestDirectory.Replace('\','/')
$kankiLines=@(
 '(setvar "SECURELOAD" 0)',
 ('(setvar "TRUSTEDPATHS" "'+$kankiRoot+'")'),
 ('(load "'+$kankiRoot+'/07_kikitable.lsp")'),
 ('(load "'+$kankiRoot+'/06_kikiset.lsp")'),
 ('(setq kr:fixture "'+$kankiDir+'/fixture.xlsx" kr:output "'+$kankiDir+'/result.xlsx")'),
 ('(load "'+$kankiRoot+'/kanki_calc_regression.lsp")'),
 ('(setq kr:failure-fixture "'+$kankiDir+'/failure.xlsx")'),
 ('(load "'+$kankiRoot+'/kanki_failure_regression.lsp")'),
 '(kt:cleanup)', '_.QUIT', '_Y'
)
if($ChartOnly) {
 $kankiLines=@('(setvar "SECURELOAD" 0)',('(load "'+$kankiRoot+'/07_kikitable.lsp")'),
  ('(setq kr:fixture "'+$kankiDir+'/fixture.xlsx")'),('(load "'+$kankiRoot+'/kanki_chart_regression.lsp")'),
  '(kt:cleanup)','_.QUIT','_Y')
}
if($HeaderOnly) {
 $kankiLines=@('(setvar "SECURELOAD" 0)',('(load "'+$kankiRoot+'/07_kikitable.lsp")'),
  ('(setq kr:fixture "'+$kankiDir+'/fixture.xlsx")'),('(load "'+$kankiRoot+'/kanki_header_regression.lsp")'),
  '(kt:cleanup)','_.QUIT','_Y')
}
if($Ver03Only) {
 $kankiLines=@('(setvar "SECURELOAD" 0)',('(load "'+$kankiRoot+'/07_kikitable.lsp")'),
  ('(setq kr:fixture "'+$kankiDir+'/fixture.xlsx")'),('(load "'+$kankiRoot+'/kanki_ver03_regression.lsp")'),
  '(kt:cleanup)','_.QUIT','_Y')
}
$kankiScript=Join-Path $TestDirectory 'regression.scr'
[IO.File]::WriteAllLines($kankiScript,$kankiLines,[Text.UTF8Encoding]::new($false))
$kankiLog=Join-Path $TestDirectory 'console.log'
$kankiArgs='/s "'+$kankiScript+'" /l ja-JP /isolate KANKI_CALC_TEST "'+(Join-Path $TestDirectory 'profile')+'"'
$kankiSentinel=New-Object -ComObject Excel.Application
$kankiSentinel.DisplayAlerts=$false
$kankiSentinelBooks=$kankiSentinel.Workbooks
$kankiSentinelBook=$kankiSentinelBooks.Add()
$kankiSentinelSheet=$kankiSentinelBook.Worksheets.Item(1)
$kankiSentinelRange=$kankiSentinelSheet.Range('A1')
$kankiSentinelRange.Value2='USER_INSTANCE_KEEP'
try {
 $kankiProcess=Start-Process -FilePath 'C:\Program Files\Autodesk\AutoCAD 2025\accoreconsole.exe' -ArgumentList $kankiArgs -WindowStyle Hidden -PassThru -RedirectStandardOutput $kankiLog
 $kankiProcess.WaitForExit()
 if($kankiSentinelBooks.Count -ne 1 -or $kankiSentinelRange.Value2 -ne 'USER_INSTANCE_KEEP') { throw 'Other Excel instance was modified' }
 Write-Output 'PASS: other Excel instance survived unchanged'
} finally {
 $kankiSentinelBook.Close($false)
 $kankiSentinel.Quit()
 foreach($kankiCom in @($kankiSentinelRange,$kankiSentinelSheet,$kankiSentinelBook,$kankiSentinelBooks,$kankiSentinel)) {
  [Runtime.InteropServices.Marshal]::FinalReleaseComObject($kankiCom) | Out-Null
 }
}
Get-Content -LiteralPath $kankiLog -Encoding Unicode | Select-Object -Last 65
$kankiText=Get-Content -LiteralPath $kankiLog -Encoding Unicode -Raw
if($kankiText -match 'FAIL:|REGRESSION_ERROR:') { throw 'AutoCAD regression failed' }
if($Ver03Only) {
 if($kankiText -notmatch 'KANKI_VER03_PASS=\d+') { throw 'ver03 regression did not finish' }
} elseif($HeaderOnly) {
 if($kankiText -notmatch 'KANKI_HEADER_PASS=12') { throw 'Header COM regression did not finish' }
} elseif($ChartOnly) {
 if($kankiText -notmatch 'KANKI_CHART_PASS=2') { throw 'Chart COM regression did not finish' }
} elseif($kankiText -notmatch 'KANKI_REGRESSION_PASS=' -or $kankiText -notmatch 'KANKI_FAILURE_PASS_TOTAL=') { throw 'One or more AutoCAD regression suites did not finish successfully' }
if((Get-FileHash -LiteralPath (Join-Path $TestDirectory 'failure.xlsx')).Hash -ne $kankiOriginalHash) { throw 'Failure fixture was saved unexpectedly' }
if(($HeaderOnly -or $ChartOnly) -and (Get-FileHash -LiteralPath (Join-Path $TestDirectory 'fixture.xlsx')).Hash -ne $kankiOriginalHash) { throw 'Read-only test fixture was saved unexpectedly' }
Write-Output 'PASS: all failure transactions left file SHA256 unchanged'
