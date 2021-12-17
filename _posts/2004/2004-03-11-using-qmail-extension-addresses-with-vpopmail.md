---
title: "Using qmail extension addresses with vpopmail"
date: 2004-03-11T10:53:00.000Z
redirect_from: /node/view/172
tags: qmail
layout: series
series: qmail
---
Part 6 of [Installing qmail and vpopmail](/node/view/165).

## Extension addresses

Quite often, I'll hand out a qmail extension address (i.e. `roger-foo@domain.com`) so that I can more easily filter email received to this account. This is useful. According to the `vpopmail` documentation, the `--enable-qmail-ext` switch to `configure` is supposed to turn this on. Unfortunately, it doesn't appear to do anything. This is with vpopmail-5.4.0\. I'm not entirely sure what it's supposed to, but it doesn't make extension addresses work.

The fix is relatively simple (although not particularly nice): Modify the `vadduser` replacement script, given previously, so that it creates a `.qmail-default` file for the user:

<pre>#!/bin/sh

/home/vpopmail/bin/vadduser.real $1 $2

# Set up the IMAPdir stuff for BincIMAP...
VDIR=`/home/vpopmail/bin/vuserinfo -d $1`

mkdir -p $VDIR/IMAPdir
chown vpopmail.vchkpw $VDIR/IMAPdir
chmod 2700 $VDIR/IMAPdir
ln -s ../Maildir $VDIR/IMAPdir/INBOX

# Set up the .qmail-default file, so that extension addresses work correctly...
VUSER=`/home/vpopmail/bin/vuserinfo -n $1`
echo "./$VUSER/Maildir/" > $VDIR/../.qmail-$VUSER-default</pre>

There might be a better fix, but I've not found it yet.

Next: [Forwarding addresses to another account using vpopmail](/node/view/173).
