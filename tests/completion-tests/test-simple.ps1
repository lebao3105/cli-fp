# Simple test
$result = & ".\SimpleDemo.exe" __complete "greet" "--verbose" ""
Write-Host "Result: $result"
Write-Host "Result type: $($result.GetType().FullName)"
Write-Host "Result count: $($result.Count)"
