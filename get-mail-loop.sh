#!/bin/sh

set -x
PATH=~/bin:$PATH

fm() {
feed2maildir --feed $1 --maildir ~/mail/rss
}


fetchrss() {
fm "https://www.phoronix.com/rss.php"
fm "https://lwn.net/headlines/rss"
fm "https://timmydouglas.com/post/index.xml"
fm "https://news.ycombinator.com/rss"
fm "https://drewdevault.com/blog/index.xml"
fm "https://www.theurbanist.org/feed/"
fm "https://www.seattlebikeblog.com/feed/"
fm "https://ln.ht/_/feed/~ddevault"
}



while true
do
    date
    mbsync -a
    fetchrss
    notmuch search --output=files --format=text0 tag:delete | xargs -r0 rm
    notmuch new 2> /dev/null
    notmuch tag --batch --input=/home/timmy/.config/notmuch/tags.txt
    sleep 5m
done
