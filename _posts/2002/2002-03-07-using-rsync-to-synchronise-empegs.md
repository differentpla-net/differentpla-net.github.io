---
title: "Using rsync to Synchronise empegs"
date: 2002-03-07T12:21:00.000Z
tags: empeg
---
## Introduction

A while ago, I published my [Lazy Bastard's Guide to Cloning your empeg]({% post_url 2002/2002-03-18-lazy-bastards-guide-to-cloning-your-empeg %}). This has worked successfully for me and other people. However, it has one major limitation -- it's an all-or-nothing operation -- it clones the entire empeg. If you add a bunch of tracks to one of your empegs, you end up having to copy _all_ of your tracks again.

In this document, I'll present an alternative: using [rsync](http://rsync.samba.org) to keep a pair of empegs in sync.

This procedure is not without its limitations -- it only works one way, so you have to upload your tracks to one of your empegs (call it the master), and then replicate it to the other (the slave). For me, this isn't a problem.

## Caveat

This is not for the faint-hearted. If you're not reasonably competent with Linux (or another Unix), then turn back now.

## The Plan

I originally attempted to get rsync working [using rsh](/rsh-client.html). However, I couldn't get a rsh server working on the empeg, so I gave up. I didn't try very hard, though, so I may try again later.

Instead, we'll use rsync as a daemon on one of the empegs, and connect to it from the other.

## Installing rsync

As with my [cloning guide]({% post_url 2002/2002-03-18-lazy-bastards-guide-to-cloning-your-empeg %}), we'll grab the .deb files from a Debian mirror, remove the stuff we don't need, and then send them to the empeg.

For this, you'll need the [rsync_2.3.2-1.2.deb](http://ftp.us.debian.org/debian/dists/potato/main/binary-arm/net/rsync_2.3.2-1.2.deb) file. Extract it, remove the /usr/share parts, and then pack it back up. Send this to the empeg, and then extract it. I generally store the .tar.gz files in /drive0, so that the files don't get overwritten by upgrades, and I can reinstate the software by unpacking them again.

Anyway, binaries and source are [here](/~roger/empeg/car/files/).

Install rsync on both empegs.

## rsyncd.conf

It doesn't particularly matter whether you run rsync as a daemon on the source empeg or on the destination empeg. In this case, we'll run it on the source empeg.

The first thing we need is a configuration file. This should be named `/etc/rsyncd.conf`:

<pre>log file = /drive0/rsyncd.log
pid file = /drive0/rsyncd.pid
uid = root
gid = root

[drive0]
    path = /drive0
    use chroot = no
    read only = no

[drive1]
    path = /drive1
    use chroot = no
    read only = no</pre>

If you're planning on running this on the source empeg, then you can remove the "read only" lines.

## hostname

Because rsync calls `uname(2)` to find out the hostname of the empeg, you'll need to set it first. If you don't, you'll see:

<pre>gethostbyname: "Unknown host" (none)</pre>

in the log file.
The _easy_ way to fix it is to put some stuff in /etc/hosts, like this:

<pre>127.0.0.1 localhost
my-empeg-ip my-empeg-name (none)</pre>

The _right_ way to fix it is to install `hostname`, from [hostname_2.07.deb](http://ftp.us.debian.org/debian/dists/stable/binary-arm/base/hostname_2.07.deb), and then to use this to set the hostname of the empeg that will be running rsync as a daemon. You can grab a copy from [here](/files/).

Even if you fix it the right way, you'll still need to put some stuff in /etc/hosts, but you can forget the "(none)" hack:

<pre>127.0.0.1 localhost
192.168.0.1 my-empeg my-empeg.whatever.tld</pre>

Note that the hostname you set with `hostname` must be listed in /etc/hosts, or resolvable via DNS.
## uid/gid

Because we've specified a uid and gid in rsyncd.conf, we need to make sure that these IDs resolve properly. If we don't do this, we'll see "@ERROR: invalid uid" at the client, with a corresponding "Invalid uid root" message in the log file; or the corresponding errors, but with "gid" instead of "uid".

To fix it:

<pre>$ **echo "root::0:0:root:/:/bin/sh" >> /etc/passwd**
$ **echo "root:x:0:" >> /etc/group**</pre>

## Start the rsync daemon

<pre>source-empeg:/# **rsync --daemon**</pre>

Note: check that it's running (using ps), because the rsync daemon isn't particularly talkative when it bails. If it's not running, check the log file, and then have a look in the troubleshooting section, below.

## Start the rsync client

<pre>dest-empeg:/drive0# **rsync -auv --delete rsync://source-empeg/drive0/fids .**</pre>

Do the same, but for drive1.
Note that the **--delete** removes files from the destination if they're not present on the source. You might want to throw a **-n** (dry run -- display actions without doing them) in there first.

## Rebuild the database

Remove the `database`, `playlists`, `tags` files from `/empeg/var` on the destination empeg. Restart the player. It'll rebuild the database.

# Troubleshooting

## neighbour table overflow

If you get

<pre># **ping toothgnip**
neighbour table overflow
ping: sendto: No buffer space available
ping: wrote toothgnip 64 chars, ret=-1</pre>

...then do this:
<pre># **ifconfig lo 127.0.0.1**</pre>
