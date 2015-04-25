---
title: "Linux on Sony Vaio - Firewire DVD-ROM drive"
date: 2003-02-04T14:47:00.000Z
x-drupal-nid: 136
x-needs-review: 2003-02-04T14:47:00.000Z
---
To get the DVD-ROM drive working, you'll need to compile your kernel with the following options:

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

You don't need any of the SCSI low-level drivers.

This should be sufficient to support the Firewire controller, and to support the DVD-ROM drive. Build and install your [new kernel](/drupal-4.7.3/make-kpkg.html), and reboot.

Connect the DVD-ROM drive and put in a CD-ROM or DVD-ROM (or a DVD video or audio disc).

Then, as root:

<div class="snippet">
    # modprobe ohci1394
    # modprobe sbp2
    # mount /dev/scd0 /mnt

</div>

Your CD or DVD should now be visible in `/mnt`.