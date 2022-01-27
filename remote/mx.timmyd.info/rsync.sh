#!/bin/sh

rsync -avz -e "ssh -i ~/.aws/aws1.pem" etc root@mx.timmyd.info:/
