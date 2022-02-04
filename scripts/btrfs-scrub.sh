#!/bin/sh

sudo btrfs scrub start -Bd | mail -s "btrfs report: $(date +%F)" timmy
