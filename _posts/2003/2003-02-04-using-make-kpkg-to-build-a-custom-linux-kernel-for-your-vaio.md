---
title: "Using make-kpkg to build a custom Linux kernel for your VAIO"
date: 2003-02-04T15:10:00.000Z
layout: series
series: linux-on-vaio
tags: linux sony-vaio
---

Debian provides a powerful utility called `make-kpkg` for compiling your own custom kernels. It integrates tightly with
dpkg by making `.deb` files and you can even use this to prepare your kernel image on a faster system. Later on it will
make complex operations, such as compiling in patches and modules, a snap.

To get it:

```
# apt-get install kernel-package libncurses-dev
```

`libncurses-dev` is not strictly necessary, but provides the libraries we will use later for configuring the kernel in
the easy to use curses interface (`make menuconfig`).

You will also need a copy of the kernel source. You can download it directly from
`ftp._xx_.kernel.org/pub/linux/kernel/v2.4` (substitute your two-letter country code for `xx`) or you can install a
debianised version from your local repositary. The Debian package (`kernel-source-2.4.20` - or latest since) contains a
few extra patches useful for a debian system.

The debian package will install the latest version of the source to the `/usr/src` directory. You will need to place it
there yourself, if you download it directly off a kernel mirror.

Change to the `/usr/src` directory and check the source is there before un-tarring it with:

```
$ tar -zxf kernel-source.2.4.20.tar.gz .
```

It is also good form to create a soft link to the default source working directory:

```
$ ln -s kernel-source.2.4.20 linux
```

We shall assume you created the link for the rest of the instructions. Now change to the `/usr/src/linux` directory and
being configuring your kernel:

```
$ make menuconfig
```

Prepare your kernel for later chapters by [installing the ACPI patches]({% post_url 2003/2003-01-13-vaio-acpi-patches %}),
and by setting the following sets of kernel options now:

[Firewire DVD-ROM drive]({% post_url 2003/2003-02-04-linux-on-sony-vaio-firewire-dvd-rom-drive %}):

```
CONFIG_SCSI=m (SCSI Support ---> SCSI Support)
CONFIG_BLK_DEV_SR=m (SCSI Support ---> SCSI CD-ROM support)
CONFIG_IEEE1394=m (IEEE 1394 ---> IEEE 1394)
CONFIG_IEEE1394_OHCI1394=m (IEEE 1394 ---> OHCI-1394)
CONFIG_IEEE1394_SBP2=m (IEEE 1394 ---> SBP-2)
CONFIG_ISO9660_FS=m (File systems ---> ISO 9660)
CONFIG_JOLIET=y (File systems ---> Microsoft Joliet)
CONFIG_ZISOFS=y (File systems ---> Transparent Decompression extension)
CONFIG_ZISOFS_FS=m (automatic)
CONFIG_ZLIB_INFLATE=m (automatic)
```

[Wireless Networking]({% post_url 2003/2003-01-14-linux-on-sony-vaio-built-in-wireless %}):

```
CONFIG_PCMCIA=m
CONFIG_NET_RADIO=y
CONFIG_HERMES=m
CONFIG_PCMCIA_HERMES=m
CONFIG_NET_WIRELESS=y
```

External Firewire hard disks:

```
CONFIG_BLK_DEV_SD=m (SCSI Support ---> SCSI disk support)
CONFIG_SD_EXTRA_DEVS=40 (automatic)
CONFIG_CHR_DEV_SG=m (SCSI Support ---> SCSI generic support)
```

[Apple iPod]({% post_url 2003/2003-02-04-linux-on-sony-vaio-connecting-an-ipod %}) (experimental support):

This is optional. If you don't have an Apple iPod, then you needn't set these options.

```
CONFIG_IEEE1394_RAWIO=m (IEEE 1394 ---> Raw IEEE1394 I/O support)
CONFIG_PARTITION_ADVANCED=y (File systems ---> Partition Types ---> Advanced partition selection)
CONFIG_MAC_PARTITION=y (File systems ---> Partition Types ---> Macintosh partition map support)
```

[Intel i820 onboard soundcard]({% post_url 2003/2003-01-17-linux-on-sony-vaio-intel-i820-onboard-soundcard %}):

```
CONFIG_SOUND=m (Sound ---> Sound card support)
CONFIG_SOUND_OSS=m (Sound ---> OSS sound modules)
```

Then, compile the `.deb`:

```
$ make-kpkg clean
$ fakeroot make-kpkg --revision=whatever kernel_image
```

It is essential to run the first command to synchronize the `make-kpkg` scripts. A sensible revision naming scheme would
be to replace `whatever` with a description of the destination machine and an internal version number eg, `srx87.1.0`.

Once the kernel has finished compiling it should leave (in the parent directory), a file called
`kernel-image-2.4.20_whatever_i386.deb`. As root run:

```
# dpkg -i kernel-image-2.4.20_whatever_i386.deb
```

**Note:** This doesn't build you an initial ramdisk, so your hard disk driver (IDE) and initial filesystem (ext3) will
need to be compiled into the core kernel binary. I (Roger) wasted an entire Sunday rebuilding my Vaio when I forgot
this.

**Credits:** Caoilte O'Connor wrote this page of the instructions. Thanks for that, Caoilte.
