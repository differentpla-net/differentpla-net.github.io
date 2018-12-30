---
title: "Installing BincIMAP"
date: 2004-03-11T09:35:00.000Z
redirect_from: /node/view/171
---
Part 5 of [Installing qmail and vpopmail](/node/view/165).

## Configuring BincIMAP

Another of my requirements is that my email should be accessible using IMAP. I've had a quick look at Courier IMAP, and (IMO) it's a bit oversized for my needs. So, I'm going to install [BincIMAP](http://www.bincimap.org/) instead. I grabbed the 1.2.7beta5 tarball and unpacked it in `/usr/local/src`.

Compiling it is easy:

<pre># cd /usr/local/src/bincimap-1.2.7beta5
# ./configure
# make</pre>

I think I should have got hold of a faster test box. It was a little slow (the C++ compiler uses a lot of memory, which causes paging on a box with only 64Mb of memory).

<pre># make install
# ln -s /var/opt/bincimap/service/* /service</pre>

And it's running.
## Configuring BincIMAP to use vchkpw

Nothing particularly exciting here. Just some changes to the startup scripts. This is `/var/opt/bincimap/service/imap/run`:

<pre>#!/bin/sh
exec 2>&1

exec    tcpserver -c 100 -u 0 -g 0              \
    -l $(hostname) -HDRP                        \
    0 143                                       \
    /opt/bincimap/bin/bincimap-up                       \
    --logtype=multilog                          \
    --conf=/etc/opt/bincimap/bincimap.conf --   \
    /home/vpopmail/bin/vchkpw                   \
    /opt/bincimap/bin/bincimapd</pre>

...and the corresponding change to the `imaps/run` script.
To make it easier to start and stop BincIMAP, I've added the relevant stuff to the `qmailctl` script, originally from LWQ.

## Testing it

To test it, I used Outlook Express:

<table border="1">
<tbody>
<tr>
<td>Display Name:</td>

<td>Roger Lipscombe</td>

</tr>

<tr>
<td>Email Address:</td>

<td>roger@beerology.test</td>

</tr>

<tr>
<td>POP3 Server:</td>

<td>flimsy.beerology.test</td>

</tr>

<tr>
<td>IMAP Server:</td>

<td>flimsy.beerology.test</td>

</tr>

<tr>
<td>User ID:</td>

<td>roger@beerology.test</td>

</tr>

<tr>
<td>Password:</td>

<td>password</td>

</tr>

</tbody>

</table>

Unfortunately, it doesn't work initially -- the server responds with:

<pre>LOGIN failed: Plain text password authentication is disallowed.
Please try enabling SSL or TLS in your mail client.</pre>

For now, I'm going to configure BincIMAP to allow plain text authentication, and then I'll look at tightening it up later.

BincIMAP keeps its configuration file in `/etc/opt/bincimap/bincimap.conf`. The setting we need to change is called "allow plain auth in non ssl" and it's set to "no" by default. I'll change it to "yes"

This time, Outlook Express successfully connects and displays the Inbox. If I send an email to this account from somewhere else, it turns up correctly, too. Which is cool.

## Using IMAPdir

By default, BincIMAP is configured for [Maildir++](http://www.bincimap.org/bincimap-faq.html#q12) support, which is the simplest, but only allows folders to be created inside INBOX.

I'd like to be able to create folders at the same level as INBOX. To do this, I need to change BincIMAP to use "IMAPdir" mode. The change to BincIMAP is simple. In `/etc/opt/bincimap/bincimap.conf`:

<pre>Mailbox {
    depot = "IMAPdir",                           /* Use Maildir++ style
                                                    * depot. */

    type = "Maildir",                              /* only Maildir
                                                    * support */

    path = "IMAPdir",                              /* default path */</pre>

Looking at the [BincIMAP FAQ](http://www.bincimap.org/bincimap-faq.html#q12) shows that the IMAPdir mode requires some extra directories:

<pre># cd /home/vpopmail/domains/beerology.test/roger
# mkdir IMAPdir
# chown vpopmail.vchkpw IMAPdir
# chmod 2700 IMAPdir
# cd IMAPdir
# ln -s ../Maildir INBOX</pre>

At this point, it's possible to create IMAP folders at the same level as INBOX.
This is a little irritating. This needs to be done on every account created. A good fix for this would be to change `vadduser` so that it did this for us. First, we rename `vadduser` to `vadduser.real`, and then create a script to replace it:

<pre>#!/bin/sh

/home/vpopmail/bin/vadduser.real $1 $2
VDIR=`/home/vpopmail/bin/vuserinfo -d $1`

mkdir -p $VDIR/IMAPdir
chown vpopmail.vchkpw $VDIR/IMAPdir
chmod 2700 $VDIR/IMAPdir
ln -s ../Maildir $VDIR/IMAPdir/INBOX
</pre>

This script doesn't correctly handle switches passed to it. This is left as an exercise for the reader. If anyone fancies writing one, send it to me and I'll put it here.
Next: [Using qmail extension addresses with vpopmail](/node/view/172).
