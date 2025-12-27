#!/bin/bash

# Simulate what REAL Bash does when you press TAB after "--verbose "
COMP_WORDS=(./SimpleDemo.exe greet --verbose)
COMP_CWORD=3
COMP_LINE="./SimpleDemo.exe greet --verbose "

echo "COMP_WORDS length: ${#COMP_WORDS[@]}"
echo "COMP_CWORD: $COMP_CWORD"
echo "COMP_WORDS[@]: [${COMP_WORDS[@]}]"
echo "COMP_WORDS[3]: [${COMP_WORDS[3]}]"
