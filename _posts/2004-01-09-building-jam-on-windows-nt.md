---
title: "Building Jam on Windows NT"
date: 2004-01-09T10:28:00.000Z
x-drupal-nid: 115
x-needs-review: 2004-01-09T10:28:00.000Z
---
First, you'll need to download jam version 2.3.2 from ftp.perforce.com in [zip format](ftp://ftp.perforce.com/pub/jam/jam-2.3.2.zip) or [as a tar.gz](ftp://ftp.perforce.com/pub/jam/jam-2.3.2.tar.gz)

Unpack the contents into a new directory. Edit the `Makefile`, and uncomment the relevant lines under the comment:

<pre># NT (with Microsoft compiler)</pre>

Tell it where your VC++ libraries live. This might not work if you've installed VC++ in the default location (spaces in the path names, see). I haven't, so I just:

<pre>set MSVCNT=P:\VStudio\VC98</pre>

Build it:

<pre>nmake -f Makefile</pre>

It's a bootstrap build process. It uses make (or nmake) to build jam, and then runs jam to build itself again. You should have a `jam.exe` file in the `bin.ntx86` directory. Copy it somewhere sensible.