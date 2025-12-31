#!/bin/bash
_myapp_completions()
{
  local cur words cword args out dir candidates
  cur="${COMP_WORDS[COMP_CWORD]}"
  words=("${COMP_WORDS[@]}")
  cword=$COMP_CWORD
  
  echo "DEBUG: cur=[$cur]" >&2
  echo "DEBUG: words=(${words[@]})" >&2
  echo "DEBUG: cword=$cword" >&2
  
  # Build args for __complete and call the application
  args=()
  for ((i=1;i<=cword;i++)); do args+=("${words[i]}"); done
  # If cursor is after a space, append empty token to indicate new word
  if [[ "${COMP_LINE: -1}" == " " ]]; then args+=(""); fi
  
  echo "DEBUG: args=(${args[@]})" >&2
  
  out=$("./SimpleDemo.exe" __complete "${args[@]}")
  
  echo "DEBUG: out=[$out]" >&2
  
  # Last line is directive in form :<number>
  dir="$(printf "%s\n" "$out" | tail -n1)"
  if [[ $dir =~ ^:([0-9]+)$ ]]; then
    candidates="$(printf "%s\n" "$out" | sed '$d')"
    directive=${BASH_REMATCH[1]}
  else
    candidates="$out"
    directive=0
  fi
  
  echo "DEBUG: candidates=[$candidates]" >&2
  echo "DEBUG: directive=$directive" >&2
  
  # Populate COMPREPLY with matching candidates
  while IFS='' read -r comp; do
    echo "DEBUG: read comp=[$comp]" >&2
    [[ -z "$comp" ]] && continue
    COMPREPLY+=("$comp")
  done < <(compgen -W "$candidates" -- "$cur")
  
  echo "DEBUG: COMPREPLY=(${COMPREPLY[@]})" >&2
  return 0
}

# Simulate completion for: ./SimpleDemo.exe greet --verbose <TAB>
COMP_WORDS=(./SimpleDemo.exe greet --verbose "")
COMP_CWORD=3
COMP_LINE="./SimpleDemo.exe greet --verbose "

_myapp_completions

echo "Final COMPREPLY has ${#COMPREPLY[@]} items:"
printf '%s\n' "${COMPREPLY[@]}"
