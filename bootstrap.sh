#!/bin/sh
set -ex
git clone https://github.com/rcy/dotfiles.git $HOME/dotfiles
set +x
echo to continue run:
echo   make -C $HOME/dotfiles
