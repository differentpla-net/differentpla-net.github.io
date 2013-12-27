---
layout: post
title: "channel_setup_fwd_listener: cannot listen to port"
date: 2013-11-01T12:24:29.025Z
tags: ssh
alias: /post/UnOe2zfES1ZOAAAB/channel_setup_fwd_listener-cannot-listen-to-port
---

I was attempting to use SSH to set up a tunnel (to our build server) this morning,
so I typed the following:

    ssh -L8080:localhost:8080 jenkins

...only to be greeted with the error:

    bind: Address already in use
    channel_setup_fwd_listener: cannot listen to port: 8080

Most odd. I'm pretty sure I don't have anything else listening on that port, but
I confirmed it with a quick `netstat -anpt`. Nope. Nothing listening on the port.

More strangely, with the SSH session open, I can go to http://localhost:8080 and
see Jenkins. When I close the SSH session, I can no longer get to the page.

*Lightbuild goes on*

I've got the following in my `~/.ssh/config` file:

    Host jenkins
      LocalForward 8080 127.0.0.1:8080
  
Doh! SSH itself had already opened the port, and was complaining that it couldn't
do it a second time.

A simple `ssh jenkins` was enough.
