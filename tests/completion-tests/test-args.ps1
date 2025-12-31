# Test what PowerShell passes
$args1 = @("__complete", "greet", "--verbose", "")
Write-Host "Args: $($args1 -join ' | ')"
$result = & ".\SimpleDemo.exe" @args1
Write-Host "Result lines:"
foreach ($line in $result) {
    Write-Host "  [$line]"
}
