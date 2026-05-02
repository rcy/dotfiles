#!/bin/bash

set -eu

now=$(date +%Y%m%d_%H%M%S)
dest="$HOME/Dropbox/CampSnap/${now}"

find /media/rcy/CampSnap/DCIM -type f | while read -r f; do
    mkdir -p "$dest"
    num=$(echo "$f" | grep -oP '\d+(?=\.\w+$)')
    ext="${f##*.}"
    cp -v "$f" "$dest/${now}_${num}.${ext}"
done

rm -rf /media/rcy/CampSnap/DCIM/*
