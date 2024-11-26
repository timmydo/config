#!/bin/sh

fm() {
~/bin/feed2maildir --feed $1 --maildir ~/mail/rss
}

fm2() {
~/bin/rss2maildir -d ~/mail/rss $1
}


fm "https://www.phoronix.com/rss.php"
fm "https://lwn.net/headlines/rss"
fm "https://timmydouglas.com/post/index.xml"
fm "https://news.ycombinator.com/rss"
fm "https://drewdevault.com/blog/index.xml"
fm "https://www.theurbanist.org/feed/"
fm "https://www.seattlebikeblog.com/feed/"
#fm2 "https://emersion.fr/blog/rss.xml"
#fm2 "https://danluu.com/atom.xml"
#fm2 "https://turtleware.eu/rss.xml"
