#!/bin/sh

set -x

while true
do
    date
    mbsync -a
    ~/bin/fetchrss.sh
    notmuch search --output=files --format=text0 tag:delete | xargs -r0 rm
    notmuchnew
    sleep 5m
done
