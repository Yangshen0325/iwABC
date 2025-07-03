#!/bin/bash


if [ -z "$1" ] || [ ! -d "$1" ]; then
    echo source directory not given or non-existent
    exit 1
fi
if [ -z "$2" ]; then
    echo destination not given or not creatable
    exit 1
fi
mkdir -p "$2"
cd "$1"
for d in */; do
    if [ 100 -eq `ls -l $1$d*.txt | wc -l` ]; then
        echo "$1$d"
        cp -r "$1$d" "$2/$d"
    fi
done
