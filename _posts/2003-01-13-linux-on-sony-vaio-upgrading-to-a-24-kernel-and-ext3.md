---
title: "Linux on Sony Vaio - Upgrading to a 2.4 kernel and ext3"
date: 2003-01-13T15:44:00.000Z
x-drupal-nid: 138
x-needs-review: 2003-01-13T15:44:00.000Z
---
## Upgrading to a 2.4 kernel

In dselect, install `kernel-image-2.4.18-686`. This will drag in all of the other required packages for a 2.4 kernel. It'll suggest `kernel-pcmcia-modules-2.4.18-686` as well. You might as well select this as well.

Let dselect install all of the selected packages. This shouldn't take too long. While configuring the new kernel image, it'll complain about a `initrd=/initrd.img` line in your `/etc/lilo.conf`. Read the instructions carefully and do what it says. When you've hacked on `/etc/lilo.conf` and `/etc/kernel-img.conf`, answer 'n' to the question; you don't want to stop.

## Reboot

Quit from dselect and reboot. `exec shutdown -r now`

When it comes back up, you should be running a 2.4 kernel. Log in as root and run `uname -a` to check.

## Upgrading to ext3

There's nothing to it:

<div class="snippet">
    # tune2fs -j /dev/hda2

</div>

Having done this, edit `/etc/fstab` so that it reads `auto`, rather than `ext2`.

<div class="snippet">
    # touch /forcefsck
    # exec shutdown -r now

</div>

The last two steps are to trigger `fsck` at the next reboot (to fix up the .journal file), and to reboot.
When it's finished rebooting, log in as root and `cat /proc/mounts` to check that your root filesystem was correctly mounted as ext3.