#!/bin/sh
systemctl start --user offlineimap-oneshot@timmydouglascom.timer
systemctl start --user offlineimap-oneshot@mailtimmydouglascom.timer
systemctl start --user offlineimap-oneshot@timmydinfo.timer     
systemctl start --user offlineimap-oneshot@gmail.timer     
systemctl start --user notmuch.timer
systemctl start --user rss.timer
