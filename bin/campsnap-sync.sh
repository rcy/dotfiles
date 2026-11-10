#!/bin/bash

set -eu

now=$(date +%Y%m%d_%H%M%S)
dest="$HOME/Dropbox/CampSnap/$(date +%Y)"

find /run/media/rcy/CampSnap/DCIM -type f | while read -r f; do
    mkdir -p "$dest"
    num=$(echo "$f" | grep -oP '\d+(?=\.\w+$)')
    ext="${f##*.}"
    cp -v "$f" "$dest/${now}_${num}.${ext}"
done

rm -rf /run/media/rcy/CampSnap/DCIM/*
