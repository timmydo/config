#!/bin/bash

# Source additional profile files
source ~/.profile

# Ensure XDG_RUNTIME_DIR exists with correct permissions
mkdir -p $XDG_RUNTIME_DIR
chmod 700 "$XDG_RUNTIME_DIR"

# ssh-agent, pinned to a fixed socket path. Letting ssh-agent pick its own random
# path bakes that path into every shell's environment, so when the agent restarts
# every already-open terminal points at a socket that no longer exists and there
# is no way to tell them the new one. ssh-add exits 2 when it cannot connect.
export SSH_AUTH_SOCK="$HOME/.ssh/agent.sock"
ssh-add -l >/dev/null 2>&1
if [ $? -eq 2 ]; then
    rm -f "$SSH_AUTH_SOCK"
    ssh-agent -a "$SSH_AUTH_SOCK" >/dev/null
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
