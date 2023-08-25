#!/bin/bash
set -exu

DIR=$((find ~/personal ~/work -maxdepth 1 -mindepth 1 -type d ; echo ~/dotfiles ) | fzf)

NAME=$(basename $DIR)

set +e
tmux new-session -d -c $DIR -s $NAME
RESULT=$?
set -e
if [ $RESULT -eq 0 ]; then
    if [ -e $DIR/shell.nix ]; then
        tmux send-keys -t $NAME 'nix-shell' C-m
    fi
fi
tmux switch-client -t $NAME
