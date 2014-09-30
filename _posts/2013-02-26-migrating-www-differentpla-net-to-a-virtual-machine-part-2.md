---
title: Migrating www.differentpla.net to a virtual machine, part 2
date: 2013-02-26T06:12:00Z
---
Yesterday, I said that I'd be migrating my website to a virtual machine;
originally, I'd planned to mess around with Linux Live CDs.

However, while creating the virtual machine in Hyper-V, I discovered that it
(at least in Windows 2012) has the ability to create a VHDX from a physical disk.
And, fortunately, I had a test PC, on which I'd installed Windows 2012 Server,
which had the case open...

So: I extracted the hard disk from the existing Linux box, plugged it into the
test PC and told Hyper-V to clone the disk. A couple of hours later, I had a
.VHDX file containing an image of my Linux web server.

Firing it up, however, revealed two problems:

1. No network connectivity.
2. Some things are really slow. In particular, console output is basically unusable.

These are both caused by the same thing: no integration components are loaded.

I found [this page](http://social.technet.microsoft.com/wiki/contents/articles/961.how-to-install-ubuntu-server-10-04-in-hyper-v.aspx),
which explains how to add the relevant modules to Ubuntu 10.04.
Unfortunately, it didn't work for me: I got a kernel panic when I ran `insmod hv_netvsc.ko`.
While [this page](http://www.vyatta.org/node/6236) isn't about Ubuntu, it would
appear that I'm having the same problem.

So, I used a "Legacy Network Adapter" instead. It wasn't `eth0`, so I had to
figure that out from:

    cat /proc/net/dev

...which told me that the adapter was called `eth1`. At this point, I could
edit the `/etc/network/interfaces` to configure the network adapter, and then
run `ifup eth1` to bring up the network.

Success!

And now to upgrade to a more-recent version of Ubuntu (from Lucid Lynx to
Precise Pangolin), so that I can get integration components that don't panic
the kernel...
