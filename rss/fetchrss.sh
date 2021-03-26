#!/bin/sh

fm() {
~/bin/feed2maildir --feed $1 --maildir ~/mail/rss
}
gh() {
~/bin/feed2maildir --feed $1 --maildir ~/mail/github
}

fm "https://www.phoronix.com/rss.php"
fm "https://lwn.net/headlines/rss"
fm "https://timmydouglas.com/post/index.xml"
fm "https://news.ycombinator.com/rss"
fm "https://drewdevault.com/feed.xml"
fm "https://emersion.fr/blog/rss.xml"
fm "https://danluu.com/atom.xml"

