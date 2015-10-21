---
title: Running rsync over an SSH port forward
date: 2015-10-21 08:26+0000
---

## Why?

I'm using bastion, and it's slow to establish connections.

## What?

Use `rsync` without the implied `-e ssh`, and tunnel over an existing SSH
connection.

So: establish an SSH connection, set up a port forward, run rsync as a daemon,
and then minimize the terminal window.

## How?

### Configuration

Create `rsyncd.conf` as follows:

    [foo]
      path = .
      use chroot = no
      read only = no

### The daemon

    ssh -L8873:localhost:8873 REMOTE_HOST
    rsync --daemon --no-detach --config=$PWD/rsyncd.conf --log-file=$PWD/rsyncd.log &
    tail -f $PWD/rsyncd.log

### The rsync

    rsync SOURCE rsync://localhost:8873/foo/
