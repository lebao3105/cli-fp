# Test PowerShell parameter passing
$args1 = @("__complete", "greet", "--verbose", "")
Write-Host "Calling with args: $($args1 -join ' | ')"
$result = & ".\test-paramcount.exe" @args1
Write-Host "Output:"
Write-Host $result
