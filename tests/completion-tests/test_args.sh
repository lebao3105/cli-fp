#!/bin/bash
COMP_WORDS=(./SimpleDemo.exe greet --verbose "")
COMP_CWORD=3

args=()
for ((i=1;i<=COMP_CWORD;i++)); do
  args+=("${COMP_WORDS[i]}")
  echo "Added arg $i: [${COMP_WORDS[i]}]" >&2
done

echo "Total args: ${#args[@]}" >&2
for ((i=0;i<${#args[@]};i++)); do
  echo "args[$i] = [${args[i]}]" >&2
done

echo "Calling: ./SimpleDemo.exe __complete \"\${args[@]}\"" >&2
out=$("./SimpleDemo.exe" __complete "${args[@]}")
echo "Output: [$out]"
