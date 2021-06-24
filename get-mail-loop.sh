#!/bin/sh

set -x

while true
do
    date
    mbsync -a
    ~/bin/fetchrss.sh
    notmuchnew
    sleep 5m
done
