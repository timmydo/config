#!/bin/sh

set -x
PATH=~/bin:$PATH

while true
do
    date
    mbsync -a
    ~/bin/fetchrss.sh
    notmuch search --output=files --format=text0 tag:delete | xargs -r0 rm
    notmuch new 2> /dev/null
    notmuch tag --batch --input=/home/timmy/.config/notmuch/tags.txt
    notmuchnew
    sleep 5m
done
