#!/bin/zsh

function_name=$1
function_args=("${@:2}")

function source_all() {
  source ~/.bashrc
  source ~/.zshrc

  [[ -f ~/.bash_profile ]] && source ~/.bash_profile
  [[ -f ~/.zsh_profile ]] && source ~/.zsh_profile

  source ~/.emacs.d/scripts/service-emacs.sh
}

source_all

echo "| function call | $function_name | $function_args"
$function_name $function_args
