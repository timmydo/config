#!/bin/sh
guix shell --pure --container --network \
     --share=$HOME/src --share=$HOME/npm --share=$HOME/.npm --share=$HOME/.claude --share=$HOME/.claude.json \
     --share=$HOME/.gemini --share=$HOME/.local \
     --expose=$HOME/.config --expose=$HOME/.npmrc \
     --expose=/var \
     --share=/etc/localtime \
     --preserve='(TERM)' \
  coreutils bash curl wget gcc-toolchain pkg-config nss-certs zlib \
  grep make which findutils sed gawk diffutils tar gzip perl git git-lfs \
  binutils nasm just m4 patch autoconf automake help2man texinfo xz \
  bzip2 mpfr gmp file ncurses readline flex bison python ninja cmake \
  node foot procps file inetutils linux-libre-headers less strace \
  guix


#-- bash -c '
#export TERM=foot
#export COLORTERM=truecolor
#export PS1="dev [\w]$ "

#claude() { node /home/timmy/npm/bin/claude "$@"; }
#gemini() { node /home/timmy/npm/bin/gemini "$@"; }

#export -f claude gemini

#bash'
