---
layout: post
title: "Installing Windows 8 Pro on Acer Aspire S7"
date: 2013-03-27T16:47:34.051Z
tags: acer-aspire-s7
alias: /post/UaZAOQFqM2ZBAAAG/installing-windows-8-pro-on-acer-aspire-s7
---

1. Make sure that the ISO file you've downloaded is not corrupt -- this caused me a bunch of grief.
2. Format a bootable USB stick as FAT32.
3. Mount the ISO.
4. Copy the files from the ISO to the USB stick.
5. In the Acer's BIOS, select UEFI; enable F12 boot menu.
6. Restart the Acer.
7. Press Fn-= (F12).
8. Select the USB stick.
9. Add the Intel drivers.
10. Continue with the installation.

Note that the 128GB SDD is two 64GB SSD drives configured as RAID0. You'll need the Intel RAID drivers.

Once you're in Windows 8, you'll need to install the wireless driver. This is available on the Acer support site.

From there, Windows can mostly figure it out.