---
title: "Linux on Sony Vaio - Intel i820 onboard soundcard"
short_title: "Intel i820 onboard soundcard"
date: 2003-01-17T14:39:00.000Z
redirect_from: /node/view/156
layout: series
series: linux-on-vaio
tags: linux sony-vaio
---
**Note:** Caoilte wrote this. I've not had a chance to check it yet.

For maximum compatability you will need to compile your kernel with the following options:

```
CONFIG_SOUND=m (Sound ---> Sound card support)
CONFIG_SOUND_OSS=m (Sound ---> OSS sound modules)
```

You do not need to set a low-level soundcard driver. We will configure that separately using the more sophisticated ALSA framework.

To setup ALSA on your laptop you will need to install some extra debian packages:

```
# apt-get install alsa-source alsa-base alsa-utils alsa-xmms dpkg-dev
```

Follow the default `debconf` prompts except for the selection of cards to be built under alsa-source. Select `intel8x0` from the list.

There should now be an `alsa-drivers.tar.gz` file in your `/usr/src` directory. Change to that directory and check before un-tarring it like this:

```
# mkdir modules
# tar -zxf alsa-drivers-tar.gz modules
```

For `make-kpkg` to find these modules you need to set the following environment variable:

```
# export MODULE_LOC="/usr/src/modules/"
```
Also append the line to your .bashrc so that it is set automatically in future (NB this assumes you use the default bash prompt).
You are now ready to recompile your kernel. Follow the instructions for [make-kpkg]({% post_url 2003/2003-02-04-using-make-kpkg-to-build-a-custom-linux-kernel-for-your-vaio %}) again, or if you have compiled with the above kernel options already change to the `/usr/src/linux` directory and run:

```
# make-kpkg modules_image
```

You should now have another `.deb` package in the parent directory called something like `alsa-modules-2.4.20_rc6+3+p0+whatever_i386.deb`. Install this:

```
# dpkg -i alsa-modules-2.4.20_rc6+3+p0+whatever_i386.deb
```

Your ALSA modules are installed. Run `modconf` as root to check (not sure - might need a reboot). Remember they are muted by default though, and that normal users cannot use them until they are added to the `audio` group in `/etc/group`. If you want to use the OSS compatability layer (for programs like realplayer) edit `/etc/alsa/alsa-base.conf`, `startosslayer=true`

**Credits:** Caoilte O'Connor wrote this page of the instructions. Thanks for that, Caoilte.
