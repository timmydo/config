#!/bin/sh

rsync -avz mta-sts.txt deploy@www.timmydouglas.com:/var/www/timmydouglas.com/.well-known/
rsync -avz mta-sts.txt timmy@www.timmydouglas.com:/var/www/mta-sts/.well-known/
