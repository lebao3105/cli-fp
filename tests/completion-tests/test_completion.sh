#!/bin/bash
source simpledemo_completion.bash

# Simulate completion for: ./SimpleDemo.exe greet --verbose <TAB>
COMP_WORDS=(./SimpleDemo.exe greet --verbose "")
COMP_CWORD=3
COMP_LINE="./SimpleDemo.exe greet --verbose "

_myapp_completions

echo "COMPREPLY has ${#COMPREPLY[@]} items:"
printf '%s\n' "${COMPREPLY[@]}"
