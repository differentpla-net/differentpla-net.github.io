---
title: "Using make-kpkg to build a custom Linux kernel for your VAIO"
date: 2003-02-04T15:10:00.000Z
x-drupal-nid: 139
x-needs-review: 2003-02-04T15:10:00.000Z
---
Debian provides a powerful utility called `make-kpkg` for compiling your own custom kernels. It integrates tightly with dpkg by making `.deb` files and you can even use this to prepare your kernel image on a faster system. Later on it will make complex operations, such as compiling in patches and modules, a snap.

To get it:

<div class="snippet">
    # apt-get install kernel-package libncurses-dev

</div>

`libncurses-dev` is not strictly necessary, but provides the libraries we will use later for configuring the kernel in the easy to use curses interface (`make menuconfig`).

You will also need a copy of the kernel source. You can download it directly from `ftp._xx_.kernel.org/pub/linux/kernel/v2.4` (substitute your two-letter country code for `xx`) or you can install a debianised version from your local repositary. The Debian package (`kernel-source-2.4.20` - or latest since) contains a few extra patches useful for a debian system.

The debian package will install the latest version of the source to the `/usr/src` directory. You will need to place it there yourself, if you download it directly off a kernel mirror.

Change to the `/usr/src` directory and check the source is there before un-tarring it with:

<div class="snippet">
    $ tar -zxf kernel-source.2.4.20.tar.gz .

</div>

It is also good form to create a soft link to the default source working directory:

<div class="snippet">
    $ ln -s kernel-source.2.4.20 linux

</div>

We shall assume you created the link for the rest of the instructions.
Now change to the `/usr/src/linux` directory and being configuring your kernel:

<div class="snippet">
    $ make menuconfig

</div>

Prepare your kernel for later chapters by [installing the ACPI patches](/node/view/149), and by setting the following sets of kernel options now:

[Firewire DVD-ROM drive](/node/view/150):

<div class="snippet">
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

</div>

[Wireless Networking](/node/view/157):

<div class="snippet">
    CONFIG_PCMCIA=m
    CONFIG_NET_RADIO=y
    CONFIG_HERMES=m
    CONFIG_PCMCIA_HERMES=m
    CONFIG_NET_WIRELESS=y

</div>

External Firewire hard disks:

<div class="snippet">
    CONFIG_BLK_DEV_SD=m (SCSI Support ---> SCSI disk support)
    CONFIG_SD_EXTRA_DEVS=40 (automatic)
    CONFIG_CHR_DEV_SG=m (SCSI Support ---> SCSI generic support)

</div>

[Apple iPod](/node/view/151) (experimental support):
 This is optional. If you don't have an Apple iPod, then you needn't set these options.

<div class="snippet">
    CONFIG_IEEE1394_RAWIO=m (IEEE 1394 ---> Raw IEEE1394 I/O support)
    CONFIG_PARTITION_ADVANCED=y (File systems ---> Partition Types ---> Advanced partition selection)
    CONFIG_MAC_PARTITION=y (File systems ---> Partition Types ---> Macintosh partition map support)

</div>

[Intel i820 onboard soundcard](/node/view/156):

<div class="snippet">
    CONFIG_SOUND=m (Sound ---> Sound card support)
    CONFIG_SOUND_OSS=m (Sound ---> OSS sound modules)

</div>

Then, compile the `.deb`:

<div class="snippet">
    $ make-kpkg clean
    $ fakeroot make-kpkg --revision=whatever kernel_image

</div>

It is essential to run the first command to synchronize the `make-kpkg` scripts. A sensible revision naming scheme would be to replace `whatever` with a description of the destination machine and an internal version number eg, `srx87.1.0`.

Once the kernel has finished compiling it should leave (in the parent directory), a file called `kernel-image-2.4.20_whatever_i386.deb`. As root run:

<div class="snippet">
    # dpkg -i kernel-image-2.4.20_whatever_i386.deb

</div>

**Note:** This doesn't build you an initial ramdisk, so your hard disk driver (IDE) and initial filesystem (ext3) will need to be compiled into the core kernel binary. I (Roger) wasted an entire Sunday rebuilding my Vaio when I forgot this.

**Credits:** Caoilte O'Connor wrote this page of the instructions. Thanks for that, Caoilte.
