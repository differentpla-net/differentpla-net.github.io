---
title: "Asus A7V Linux Box"
date: 2000-12-08T00:02:00.000Z
tags: hardware pc-builds
---
Having been so impressed with the speed of Linux on the A7V (see [here]({% post_url 2000-12-08-asus-a7v-linux-box %})), I accidentally slipped while in [Cambridge Computers](http://www.cambridge-computers.com/) and bought another one.

It installed well enough. There were just two problems:

* The D-Link DFE 530TX network card.
* The Promise ATA100 controller.

## D-Link DFE-530TX

I couldn't get Linux to install using this network card. Some discussion in the office revealed that the card might actually be either a VIA-Rhine, or a Realtek RTL8139\. However, it turns out that it's actually a Via-Rhine 2, for which support doesn't exist in the stock 2.2.17 kernel (at least not the one with Debian 2.2r1).

In the interim, however, I've stuck a DEC Tulip in the box. This seems to be working fine.

See [here](http://www.scyld.com/network) for more info.

_Update 2000-12-12:_ I spoke too soon. I can't get the D-Link card to work in Win2k, so I'm going to put the reshuffle my network cards, so that the D-Link is in the new box, and something with some driver support is in the Win2k box. Then I'll install the patched driver.

_Update 2000-03-25:_ Linux 2.2.19 is out. It supports these cards native. Hurrah. The card works now, so I stuck it in my firewall box.

## Promise ATA100 Controller

This card isn't supported out of the box on Linux 2.2.17 either. As you can probably imagine, this makes the installation trickier than it otherwise could be.

It's not too bad. Simply:

1.  Plug the disk into the UDMA66 controller on the A7V, and install to that.
2.  Download and apply the patches from [http://www.linux-ide.org/](http://www.linux-ide.org/).
3.  Build a kernel with support for the Promise controller. Don't worry about "boot off-board chipsets first".
4.  Stick your existing kernel onto a floppy disk, just in case.
5.  Edit /etc/lilo.conf and /etc/fstab, changing hda->hde, hdb->hdf, etc.
6.  Reboot.

Obviously, YMMV. It worked for me -- I've got an Asus CD-ROM drive on /dev/hda (Linux complains if there isn't anything there, it would seem), and a pair of IBM disks on /dev/hde and /dev/hdg.
_Update 2001-01-15:_ Once again, the spectre of dodgy PSUs comes back to haunt me. See [here](/node/view/144) for more information.

_Update 2001-06-25:_ I'd just like to point out that this board is the original A7V. It doesn't use the VIA 686B Southbridge that people have been complaining about so much recently.
