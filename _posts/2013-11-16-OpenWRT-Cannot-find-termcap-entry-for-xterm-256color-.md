---
layout: post
title: "OpenWRT: Cannot find termcap entry for 'xterm-256color'."
date: 2013-11-16T10:50:34.990Z
tags: openwrt
alias: /post/UodOaHA__90kAAAB/openwrt-cannot-find-termcap-entry-for-xterm-256color-
---

I've got my `TERM` environment variable set to `xterm-256color`
(even though I'm using **gnome-terminal**, but that's a different story).
When I ssh into my OpenWRT router, I get presented with the error message:

> Cannot find termcap entry for 'xterm-256color'.

The fix is simple:

    cd /usr/share/terminfo/x
    ln -s xterm-color xterm-256color
    
