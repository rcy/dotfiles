#!/bin/bash
set -eu

DIR=$(( find ~/personal ~/contrib -maxdepth 1 -type d; find ~/remote ~/work -maxdepth 2 -type d; echo ~/dotfiles ) | fzf)

NAME=$(basename $DIR | tr . _)

set +e
tmux new-session -d -c $DIR -s $NAME
RESULT=$?
set -e
if [ $RESULT -eq 0 ]; then
    if [ -e $DIR/shell.nix ]; then
        tmux send-keys -t $NAME 'cached-nix-shell' C-m
    fi
fi
tmux switch-client -t $NAME
