#!/bin/bash
COMP_WORDS=(./SimpleDemo.exe greet --verbose "")
echo "Word 3 is: [${COMP_WORDS[3]}]"
echo "Word 3 length: ${#COMP_WORDS[3]}"
