#!/bin/sh

rsync -avz -e "ssh -i ~/.aws/aws1.pem" ~/.terminfo timmy@mx.timmyd.info:~/
rsync -avz ~/.terminfo 10.18.11.2:~/
rsync -avz ~/.terminfo 10.18.11.4:~/
