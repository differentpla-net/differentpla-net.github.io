---
layout: post
title: "Capturing network traces with OpenWRT"
date: 2013-11-13T09:42:22.021Z
tags: openwrt tcpdump
alias: /post/UoNJ5wx_NVYVAAAB/capturing-network-traces-with-openwrt
---

This is mostly a reminder to myself.

## Install tcpdump

    opkg install tcpdump

## External USB Modules

I don't want to waste internal storage on the capture files, so install the USB
storage modules:

    opkg install kmod-usb-storage block-mount
    
You'll also need a filesystem driver:
    
    opkg install kmod-fs-vfat

Use 'vfat' because -- let's face it -- you'll be using a Windows-formatted USB
stick, whether you're on Windows, Linux or Mac OS X.

And you'll need some code page modules:

    opkg install kmod-nls-cp437 kmod-nls-iso8859-1

If you don't do this, then you'll get:

    mount: mounting /dev/sda1 on /mnt failed: Invalid argument

...when you attempt to mount the device later; and your log (use `logread -f &`)
will have one or both of the following:

    FAT-fs (sda1): codepage cp437 not found
    FAT-fs (sda1): IO charset iso8859-1 not found

You might see different codepage or charset warnings; just install the relevant
modules.

## Then what?

...then plug in a USB stick. Make sure there's plenty of space on it.

You'll need to mount it (assuming it's on `/dev/sda1`):

    mount /dev/sda1 /mnt -t vfat

## Capture some traces

    tcpdump -i wlan0-1 -w /mnt/dump.cap

Here `wlan0-1` is my guest WiFi. Yours might well be called something different.

## Yoink it back to a proper PC

    scp openwrt:/mnt/dump.cap .

## Examine it in Wireshark

*Left as an exercise for the reader*
