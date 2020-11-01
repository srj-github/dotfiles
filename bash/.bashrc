#  ____
# |_  /_ _ __ _
#  / /| '_/ _` |
# /___|_| \__, |
#         |___/
# . bashrc file
# . . . . . . . . . . . . . . . . . . . . .

# Imports

if [ -f ~/.bash_aliases ]; then
. ~/.bash_aliases
fi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Only load Liquid Prompt in interactive shells, not from a script or from scp
[[ $- = *i* ]] && source ~/bin/liquidprompt/liquidprompt

export PATH="$HOME/bin:$PATH"

# Vi mode
set -o vi
