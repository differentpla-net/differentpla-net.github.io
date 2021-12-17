---
title: Migrating www.differentpla.net to a virtual machine
date: 2013-02-24T16:56:39Z
---
As part of implementing my *new* blog engine, I want to decommission the box that's hosting my *existing* blog. It's taking up space in the cupboard, and I've got a better use in mind for the hardware.

I've just spent a few hours attempting to migrate everything piecemeal, in order to get a cleaner image.

However, this is taking far too long, and I'll never be sure that I've done it right. I don't want to spend too much time on this, because (as I've said) I'm going to kill it with fire soon anyway.

Instead, I'm going to do a straight physical-to-virtual migration. This involves (essentially) cloning the existing disk into a virtual disk and bringing it up in Hyper-V (in my case; VMWare or VirtualBox would work just as well).

So, here's the plan:

1. Get the machine out of the cupboard, and put it on my desk, where I can plug a monitor and keyboard into it, just in case.
2. Create a VM in Hyper-V, but don't install anything on it.
3. Boot a live image of Linux on the VM.
4. Using rsync, copy everything across to the VM.
5. Reboot the VM.

There'll probably be some messing around with repairing the bootloader, but I think this might be the easiest way.

Here goes...
