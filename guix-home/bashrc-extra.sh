#!/bin/bash

# Source additional profile files
source ~/.profile

# Ensure XDG_RUNTIME_DIR exists with correct permissions
mkdir -p $XDG_RUNTIME_DIR
chmod 700 "$XDG_RUNTIME_DIR"

# ssh-agent management
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent -t 1h > "$HOME/ssh-agent.env"
fi
if [[ ! "$SSH_AUTH_SOCK" ]]; then
    source "$HOME/ssh-agent.env" >/dev/null
fi

# Git branch in prompt
parse_git_branch() {
    git branch 2>/dev/null | sed -n 's/^\* //p'
}

# Set window title
PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}:${PWD}\007"'

# Prompt with git branch
PS1='\[\e[34m\]\w \[\e[32m\]($(parse_git_branch))\[\e[0m\] [\u@\h]\[\e[33m\] [\D{%Y/%m/%d %H:%M:%S}]\n\[\e[0m\]\$ '

# History settings
HISTFILE="$HOME/.bash_history"
HISTSIZE=50000
HISTFILESIZE=10000
HISTCONTROL=ignoreboth:erasedups
shopt -s histappend
