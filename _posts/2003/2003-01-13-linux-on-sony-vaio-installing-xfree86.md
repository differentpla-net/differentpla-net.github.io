---
title: "Linux on Sony Vaio - Installing XFree86"
short_title: "Installing XFree86"
date: 2003-01-13T15:44:00.000Z
layout: series
series: linux-on-vaio
tags: linux sony-vaio
---
## XFree86

According to [this page](http://jhecking.netgaroo.com/asus_m1300.html), the
Debian xserver-xfree86 package includes support for the Intel i815 chipset in
the SRX87, so grab that using dselect or similar.

Walk through the configuration options. You want to select the i810 option, and
answer "Yes" when it asks if you've got an LCD display. Tell it to only support
1024x768\. Otherwise, the defaults should do. The mouse is PS/2 compatible and
should be available on `/dev/psaux`.

Since startx isn't installed, you'll also want to install some kind of
`xdm`-like program. I chose `xdm`, in fact. It requires/suggests a bunch of
other stuff. Accept the defaults.

I tried starting X by simply running `xdm`, but the screen flickered a lot and
nothing happened. Pressing Ctrl+C a couple of times eventually returned me to
the prompt. Looking at `/var/log/xdm.log` revealed that I had no fonts
installed.

So you'll also need a font server and some fonts. I installed `xfs-xtt` and
`xfonts-base`.

Try starting X:

    # xdm

You should get a login prompt. It won't be pretty, though:

![](/images/1a5b10afc983b9a91e9bbb8b2027cec5-164.jpg)

Check that the touchpad moves the mouse pointer and that you can log in.

Unfortunately, I couldn't log in at this point -- it just returned to the log
in screen. Further investigation revealed the following message in
`/home/roger/.xsession-errors`:

    Xsession: unable to start X session;
    no /home/roger/.xsession file, no /home/roger/.Xsession file,
    no session managers, no window managers, and no terminal emulators found.
    Aborting.

Enough of that, I installed `gdm` using dselect, because I figured that it'd
drag in a session manager. It drags in a whole heap of other stuff as well,
though. Time to make a cup of tea.

I had to grab a GNOME-compatible window manager and `gnome-terminal`, and
install `gnome-control-center`, but after that, it was more or less usable.
