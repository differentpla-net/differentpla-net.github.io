---
title: "Upgrading RaspiOS to arm64"
date: 2021-12-20T10:26:00Z
tags: raspberry-pi
---

Can I upgrade my Raspberry Pi 4-powered k3s cluster to arm64? Without rebuilding everything? tl;dr: no.

```
$ uname -a
Linux rpi405 5.10.63-v7l+ #1496 SMP Wed Dec 1 15:58:56 GMT 2021 armv7l GNU/Linux
```

```
sudo rpi-update
```

```
sudo vi /boot/config.txt
```

In the `[pi4]` section, add `arm_64bit=1`. Reboot.

```
$ uname -a
Linux rpi405 5.10.87-v8+ #1502 SMP PREEMPT Fri Dec 17 15:15:12 GMT 2021 aarch64 GNU/Linux

$ dpkg --print-architecture
armhf
```

64-bit kernel. 32-bit userland.

How to upgrade userland to 64-bit? [Just don't bother](https://forums.raspberrypi.com/viewtopic.php?t=320444):

>  I have done it successfully, just for the fun of it, but it's difficult,
>  time-consuming, and really isn't worth the hassle. So much so that I haven't
>  bothered to write it up, and it was long enough ago that I forget the
>  gotchas.

> Much, much better to reinstall. The usual --get-selections | --set-selections
> dance, with a backup of /etc, is easier, cleaner, less error-prone, and much
> more likely to result in a working system. But yes: it *can* be done. Just
> don't bother.

Vladimir over at `rpi4cluster.com` [went with](https://rpi4cluster.com/k3s/k3s-design/) 64-bit Ubuntu 20.10.
