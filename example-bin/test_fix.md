# Quick Test for Root-Level Flag Completion Fix

## Test the Fix

In Git Bash, run these commands to verify the fix:

```bash
cd /c/Users/iwank/Documents/github/cli-fp/example-bin

# Source the completion script
source subcommanddemo_completion.bash

# Test 1.3: Complete '--h' prefix (THIS WAS FAILING BEFORE)
./SubCommandDemo.exe --h[TAB][TAB]
```

**Expected result:**
```
--help  --help-complete
```

**Additional tests:**

```bash
# Test all root-level flags with '--' prefix
./SubCommandDemo.exe --[TAB][TAB]
```

**Expected:**
```
--help  --help-complete  --version  --completion-file  --completion-file-pwsh
```

```bash
# Test all root-level flags with '-' prefix
./SubCommandDemo.exe -[TAB][TAB]
```

**Expected:**
```
--help  --help-complete  --version  --completion-file  --completion-file-pwsh  -h  -v
```

---

## What Was Fixed

**Before:** Typing `./SubCommandDemo.exe --h[TAB]` showed no completions

**After:** Typing `./SubCommandDemo.exe --h[TAB]` completes to `--help` or shows `--help --help-complete`

**Root Cause:** The completion logic was trying to match `--h` as a **command name** instead of recognizing it as a **flag prefix**.

**Fix:** Added logic to detect when the first token starts with `-` and complete root-level flags instead of commands.

**File Modified:** `src/cli.application.pas` (lines 1095-1110)
