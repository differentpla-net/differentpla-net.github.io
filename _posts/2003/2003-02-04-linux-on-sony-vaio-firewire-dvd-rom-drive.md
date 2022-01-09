---
title: "Linux on Sony Vaio - Firewire DVD-ROM drive"
short_title: "Firewire DVD-ROM drive"
date: 2003-02-04T14:47:00.000Z
redirect_from: /node/view/150
layout: series
series: linux-on-vaio
tags: linux sony-vaio
---
To get the DVD-ROM drive working, you'll need to compile your kernel with the following options:

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

You don't need any of the SCSI low-level drivers.

This should be sufficient to support the Firewire controller, and to support the DVD-ROM drive. Build and install your [new kernel]({% post_url 2003/2003-02-04-using-make-kpkg-to-build-a-custom-linux-kernel-for-your-vaio %}), and reboot.

Connect the DVD-ROM drive and put in a CD-ROM or DVD-ROM (or a DVD video or audio disc).

Then, as root:

```
# modprobe ohci1394
# modprobe sbp2
# mount /dev/scd0 /mnt
```

Your CD or DVD should now be visible in `/mnt`.
