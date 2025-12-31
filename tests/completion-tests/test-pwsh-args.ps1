# Debug script to see what PowerShell passes to the completer
$scriptBlock = {
  param($wordToComplete, $commandAst, $cursorPosition)

  Write-Host "=== DEBUG ===" -ForegroundColor Cyan
  Write-Host "wordToComplete: [$wordToComplete]" -ForegroundColor Yellow
  Write-Host "commandAst: [$commandAst]" -ForegroundColor Yellow
  Write-Host "cursorPosition: [$cursorPosition]" -ForegroundColor Yellow

  $line = $commandAst.ToString()
  Write-Host "line: [$line]" -ForegroundColor Yellow

  $words = $line -split " +"
  Write-Host "words: [$($words -join ', ')]" -ForegroundColor Yellow
  Write-Host "words.Count: $($words.Count)" -ForegroundColor Yellow

  $cword = $words.Count - 1
  $argsList = @()
  for ($i = 1; $i -le $cword; $i++) { $argsList += $words[$i] }
  if ($line.EndsWith(" ")) { $argsList += "" }

  Write-Host "argsList: [$($argsList -join ', ')]" -ForegroundColor Yellow
  Write-Host "=============`n" -ForegroundColor Cyan

  return @()
}

Register-ArgumentCompleter -CommandName ".\SimpleDemo.exe" -ScriptBlock $scriptBlock

Write-Host "Now try: .\SimpleDemo.exe greet --verbose <TAB>" -ForegroundColor Green
