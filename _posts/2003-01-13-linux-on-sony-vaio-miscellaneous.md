---
title: "Linux on Sony Vaio - Miscellaneous"
date: 2003-01-13T16:03:00.000Z
x-drupal-nid: 140
x-needs-review: 2003-01-13T16:03:00.000Z
---
A [page](http://life.bio.sunysb.edu/~jhajagos/linux/SRX87/) that I found recommends installing the `spicctrl` and `sjog` packages to get control over the LCD brightness and JogDial.

To get `spicctrl` working, you need the `sonypi` kernel module. Fortunately, it's installed in the stock Debian kernel.

Unfortunately, there's no entry in /dev for it. Create one with:

<div class="snippet">
    # mknod /dev/sonypi c 10 250 

</div>

Create a new file, `/etc/modutils/sonypi`, containing the following:

<div class="snippet">
    alias char-major-10-250 sonypi
    options sonypi minor=250

</div>

Run `update-modules` to rebuild `/etc/modules.conf`.

Then, you can test it like this:

<div class="snippet">
    $ spicctrl -a
    $ spicctrl -b 100
    $ spicctrl -b 255
    $ sjog

</div>

I haven't figured out how to get the jog control to come back when you next use the dial. I'll look into that later.