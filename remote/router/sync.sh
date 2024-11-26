#!/bin/sh

export ROUTER=10.18.11.4

rsync -avP --files-from=files.txt 10.18.11.4:/ ./
