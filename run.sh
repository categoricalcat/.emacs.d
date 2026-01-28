#!/usr/bin/env zsh

function_name=$1
function_args=("${@:2}")

function source_all() {
  # Default to zsh, fallback to bash
  if command -v zsh >/dev/null 2>&1; then
    source ~/.zshrc
    [[ -f ~/.zsh_profile ]] && source ~/.zsh_profile
  else
    source ~/.bashrc
    [[ -f ~/.bash_profile ]] && source ~/.bash_profile
  fi

  for file in ~/.emacs.d/scripts/*.sh; do
    echo "Sourcing $file"
    source $file
  done
}

source_all

echo "| function call | $function_name | $function_args"
$function_name $function_args
