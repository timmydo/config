#!/bin/sh
set -eu

# 172.179.0.6 = claw1 vm
for host in \
  ec2-user@mx.timmydouglas.com \
  timmy@172.179.0.6
do
  scp authorized_keys "$host":~/.ssh/authorized_keys
done
