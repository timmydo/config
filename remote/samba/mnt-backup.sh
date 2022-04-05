#!/bin/sh

sudo mkdir -p /mnt/backup
sudo mount.cifs -o user=timmy,uid=`id -u` //10.18.11.2/share /mnt/backup
