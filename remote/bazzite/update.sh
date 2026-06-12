#!/bin/sh
# Update the Bazzite boxes from here.
#
# `ujust update` on Bazzite runs topgrade: system image (rpm-ostree/bootc),
# Flatpaks, Homebrew, firmware -- then offers a (R)eboot menu. It is
# INTERACTIVE: expect a "Continue?" prompt, a sudo/polkit password, and the
# reboot menu. -t gives it a TTY so those work.
#
# If you reboot a host at the end, its ssh session closes with a non-zero
# status. We deliberately DON'T use `set -e` and we `|| true` each host so one
# box rebooting never skips the next one.
#
# Usage:
#   ./update.sh           # update both
#   ./update.sh john      # just one (by ssh-config alias)

HOSTS="${*:-john emma}"

for h in $HOSTS; do
    echo "==================== $h ===================="
    ssh -t "$h" 'ujust update' || \
        echo ">>> $h: ssh exited non-zero (likely rebooted at the end) -- continuing"
done
