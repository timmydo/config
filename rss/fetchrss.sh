#!/bin/sh

fm() {
~/bin/feed2maildir --feed $1 --maildir ~/mail/rss
}

fm2() {
~/bin/rss2maildir -d ~/mail/rss $1
}


fm2 "https://www.phoronix.com/rss.php"
fm2 "https://lwn.net/headlines/rss"
fm2 "https://timmydouglas.com/post/index.xml"
fm2 "https://news.ycombinator.com/rss"
fm2 "https://drewdevault.com/blog/index.xml"
fm2 "https://www.theurbanist.org/feed/"
#fm2 "https://emersion.fr/blog/rss.xml"
#fm2 "https://danluu.com/atom.xml"
#fm2 "https://turtleware.eu/rss.xml"
