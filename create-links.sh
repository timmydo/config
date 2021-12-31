#!/bin/sh

set -ex
mkdir -p ~/bin
ln -svf ~/.config/start-sway.sh ~/bin/start-sway
ln -svf ~/.config/cosa/run.sh ~/bin/cosa
ln -svf ~/.config/start-sway.sh ~/bin/start-sway
ln -svf ~/.config/svscan.sh ~/bin/svscan
echo "source $HOME/.config/zsh-startup.sh" > ~/.zshrc


