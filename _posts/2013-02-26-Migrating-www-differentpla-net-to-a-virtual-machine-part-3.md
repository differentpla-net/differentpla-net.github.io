---
layout: post
title: "Migrating www.differentpla.net to a virtual machine, part 3"
date: 2013-02-26T06:20:31.366Z
tags: hyper-v
alias: /post/USxUL-yvVksDAAAD/migrating-www-differentpla-net-to-a-virtual-machine-part-3
---

Last night, I kicked off an upgrade from Ubuntu 10.10 (Lucid Lynx) to 12.04
(Precise Pangolin). I am distinctly unimpressed by the upgrade process.

Last week, when upgrading my Windows 2008 R2 domain controller to Windows 2012,
I had to run two commands (to upgrade the AD schema), answer a few questions at
the beginning, and then wait 40 minutes.

Last night, when upgrading Ubuntu, I had to answer a seemingly endless number
of questions, at various stages during the upgrade; it took hours; and the
machine fell off the network several times during the process. For the first
couple of times, I was able to reconnect (and use screen to
[reattach to the upgrade session](http://serverfault.com/q/387547/7027));
the final time, it fell off the network permanently, and I had to use the
Hyper-V console.

I am not impressed.
