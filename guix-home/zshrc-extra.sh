#!/bin/sh

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

# Load version control information
autoload -Uz vcs_info
precmd() {
    vcs_info
    print -Pn "\e]0;%n@%m:%d\a"
}

preexec() {
    print -Pn "\e]0;%n@%m:%d: $1\a"
}

# Format the vcs_info_msg_0_ variable
zstyle ':vcs_info:git:*' formats '%b'

# Set up the prompt (with git branch name)
setopt PROMPT_SUBST

PROMPT='%F{blue}%d %F{green}(${vcs_info_msg_0_})%F{white} [%n@%m]%F{yellow} [%D{%Y/%m/%d %H:%M:%S}]
%F{white}$ '

# History settings
[ -z "$HISTFILE" ] && HISTFILE="$HOME/.zsh_history"
[ "$HISTSIZE" -lt 50000 ] && HISTSIZE=50000
[ "$SAVEHIST" -lt 10000 ] && SAVEHIST=10000

setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
setopt share_history          # share command history data

# Key bindings
bindkey '\e[H' beginning-of-line
bindkey '\e[F' end-of-line
bindkey "\e[3~" delete-char
