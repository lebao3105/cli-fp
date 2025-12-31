# Test script for PowerShell completion functionality
# This script tests the __complete endpoint and validates completion results

Write-Host "`n=== PowerShell Completion Test ===" -ForegroundColor Cyan
Write-Host "Testing SimpleDemo.exe completion functionality`n" -ForegroundColor Cyan

# Test 1: Command completion
Write-Host "Test 1: Command completion (should show 'greet')" -ForegroundColor Yellow
$result = & ".\SimpleDemo.exe" __complete ""
Write-Host "Result: $result" -ForegroundColor Green

# Test 2: Flag completion after 'greet'
Write-Host "`nTest 2: Flag completion after 'greet' (should show --name, -n, --count, etc.)" -ForegroundColor Yellow
$result = & ".\SimpleDemo.exe" __complete "greet" ""
Write-Host "Result: $result" -ForegroundColor Green

# Test 3: Boolean value completion
Write-Host "`nTest 3: Boolean value completion for --verbose (should show true, false)" -ForegroundColor Yellow
$result = & ".\SimpleDemo.exe" __complete "greet" "--verbose" ""
Write-Host "Result: $result" -ForegroundColor Green

# Test 4: Enum value completion
Write-Host "`nTest 4: Enum value completion for --mode (should show normal, friendly, formal)" -ForegroundColor Yellow
$result = & ".\SimpleDemo.exe" __complete "greet" "--mode" ""
Write-Host "Result: $result" -ForegroundColor Green

Write-Host "`n=== Manual Interactive Test Instructions ===" -ForegroundColor Cyan
Write-Host "To test interactive TAB completion, first reload the completion script:" -ForegroundColor White
Write-Host "  . .\simpledemo_completion.ps1" -ForegroundColor Gray
Write-Host "`nThen try these interactive tests (press TAB after each):" -ForegroundColor White
Write-Host "  1. .\SimpleDemo.exe <TAB>          (should show 'greet')" -ForegroundColor Gray
Write-Host "  2. .\SimpleDemo.exe greet -<TAB>   (should show flags like --name, --count, etc.)" -ForegroundColor Gray
Write-Host "  3. .\SimpleDemo.exe greet --verbose <TAB>  (should show true, false)" -ForegroundColor Gray
Write-Host "  4. .\SimpleDemo.exe greet --mode <TAB>     (should show normal, friendly, formal)" -ForegroundColor Gray
Write-Host ""
