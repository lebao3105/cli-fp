# Usage: ./SimpleDemo.exe --completion-file-pwsh > myapp-completion.ps1
# Then in PowerShell:
#   . ./myapp-completion.ps1
# To make it permanent, add the above line to your $PROFILE
# Usage: ./SimpleDemo.exe --completion-file-pwsh > myapp-completion.ps1
# Then in PowerShell:
#   . ./myapp-completion.ps1
# To make it permanent, add the above line to your $PROFILE
# PowerShell argument completer for SimpleDemo.exe

$scriptBlock = {
  param($wordToComplete, $commandAst, $cursorPosition)
  $line = $commandAst.ToString()
  $words = $line -split " +" | Where-Object { $_ -ne '' }
  $argsList = @($words | Select-Object -Skip 1)
  if ($line.EndsWith(" ")) { $argsList += "" }
  $out = & "./SimpleDemo.exe" __complete @argsList 2>$null
  if (-not $out) { return @() }
  # Extract directive and candidates
  $directive = 0
  $candidates = @()
  foreach ($line in $out) {
    if ($line -match "^:([0-9]+)$") {
      $directive = [int]$Matches[1]
    } else {
      $candidates += $line
    }
  }
  $results = @()
  if ($candidates.Count -eq 0) { return @() }
  foreach ($c in $candidates) {
    # Skip empty candidates
    if ([string]::IsNullOrWhiteSpace($c)) { continue }
    # Filter by prefix
    if ([string]::IsNullOrEmpty($wordToComplete) -or $c.StartsWith($wordToComplete, [StringComparison]::CurrentCultureIgnoreCase)) {
      if (($directive -band 2) -ne 0) {
        $results += [System.Management.Automation.CompletionResult]::new($c, $c, "ParameterName", $c)
      } else {
        $results += [System.Management.Automation.CompletionResult]::new($c, $c, "ParameterValue", $c)
      }
    }
  }
  return $results
}

# Register for all common invocation patterns
Register-ArgumentCompleter -CommandName "SimpleDemo.exe" -ScriptBlock $scriptBlock
Register-ArgumentCompleter -CommandName "SimpleDemo" -ScriptBlock $scriptBlock
Register-ArgumentCompleter -CommandName "./SimpleDemo.exe" -ScriptBlock $scriptBlock
Register-ArgumentCompleter -CommandName ".\SimpleDemo.exe" -ScriptBlock $scriptBlock
Register-ArgumentCompleter -CommandName ".\\SimpleDemo.exe" -ScriptBlock $scriptBlock

# Try -Native flag if PowerShell 7+
if ($PSVersionTable.PSVersion.Major -ge 7) {
  Register-ArgumentCompleter -Native -CommandName "SimpleDemo" -ScriptBlock {
    param($wordToComplete, $commandAst, $cursorPosition)
    & $scriptBlock $wordToComplete $commandAst $cursorPosition
  }
}
