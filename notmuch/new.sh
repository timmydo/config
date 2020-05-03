#!/bin/sh

nm=/usr/bin/notmuch

$nm new 2>/dev/null

$nm tag --batch --input=/home/timmy/.config/notmuch/tags.txt
