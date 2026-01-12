# Test PowerShell completion for SimpleDemo.exe

Write-Host "`n=== Testing PowerShell Completion ===" -ForegroundColor Cyan

# Test 1: Command completion
Write-Host "`nTest 1: Command completion (empty input)" -ForegroundColor Yellow
$result = & "./SimpleDemo.exe" __complete ""
Write-Host "Result: $result"

# Test 2: Flag completion
Write-Host "`nTest 2: Flag completion after 'greet -'" -ForegroundColor Yellow
$result = & "./SimpleDemo.exe" __complete "greet" "-"
Write-Host "Result: $($result -join ', ')"

# Test 3: Boolean value completion
Write-Host "`nTest 3: Boolean value completion for --verbose" -ForegroundColor Yellow
$result = & "./SimpleDemo.exe" __complete "greet" "--verbose" ""
Write-Host "Result: $($result -join ', ')"

# Test 4: Enum value completion
Write-Host "`nTest 4: Enum value completion for --mode" -ForegroundColor Yellow
$result = & "./SimpleDemo.exe" __complete "greet" "--mode" ""
Write-Host "Result: $($result -join ', ')"

# Test 5: Enum prefix matching
Write-Host "`nTest 5: Enum prefix matching for --mode f" -ForegroundColor Yellow
$result = & "./SimpleDemo.exe" __complete "greet" "--mode" "f"
Write-Host "Result: $($result -join ', ')"

Write-Host "`n=== All Tests Complete ===" -ForegroundColor Green
