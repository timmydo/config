#!/bin/sh

export EDITOR="emacs"
export MOZ_ENABLE_WAYLAND=1
export XDG_CONFIG_HOME=$HOME/.config
export XDG_RUNTIME_DIR=/tmp/timmy-xdg
mkdir -p $XDG_RUNTIME_DIR
export QT_QPA_PLATFORM=wayland
export USER=timmy

if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent -t 1h > "$XDG_RUNTIME_DIR/ssh-agent.env"
fi
if [[ ! "$SSH_AUTH_SOCK" ]]; then
    source "$XDG_RUNTIME_DIR/ssh-agent.env" >/dev/null
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
