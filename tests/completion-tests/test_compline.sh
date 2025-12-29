#!/bin/bash
COMP_LINE="./SimpleDemo.exe greet --verbose "
if [[ "${COMP_LINE: -1}" == " " ]]; then
  echo "COMP_LINE ends with space: YES"
else
  echo "COMP_LINE ends with space: NO"
fi
