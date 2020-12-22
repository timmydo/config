#!/bin/sh

nm=$(which notmuch)

$nm new 2>/dev/null

$nm tag --batch --input=/home/timmy/.config/notmuch/tags.txt
