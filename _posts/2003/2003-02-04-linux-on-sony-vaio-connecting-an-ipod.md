---
title: "Linux on Sony Vaio - Connecting an iPod"
short_title: "Connecting an iPod"
date: 2003-02-04T15:10:00.000Z
redirect_from: /node/view/151
layout: series
series: linux-on-vaio
tags: linux sony-vaio ipod
---
**Note:** This is a preliminary version of this document. I've not fully verified that the stuff in here works.

The Sony SRX87 has a 4-pin Firewire (IEEE1394 or Sony i.Link) port on the side. Next to it is the power connector for the DVD-ROM drive. However, the connector can be used by itself for connecting other firewire devices.

I decided to connect my iPod to it. I don't currently have any software for transferring to or from the iPod, but getting it to appear as a SCSI block device would be a good start.

You'll need to configure your kernel with the following options:

```
CONFIG_IEEE1394_RAWIO=m (IEEE 1394 ---> Raw IEEE1394 I/O support)
CONFIG_BLK_DEV_SD=m (SCSI Support ---> SCSI disk support)
CONFIG_SD_EXTRA_DEVS=40 (automatic)
CONFIG_CHR_DEV_SG=m (SCSI Support ---> SCSI generic support)
CONFIG_PARTITION_ADVANCED=y (File systems ---> Partition Types ---> Advanced partition selection)
CONFIG_MAC_PARTITION=y (File systems ---> Partition Types ---> Macintosh partition map support)
```

I don't know if it'll be needed, but I also turned on SCSI generic support.
Once you've compiled (and rebooted with) your new kernel, you should be able to, as root:

```
# modprobe ohci1394
# modprobe sbp2
```

These are the same modules commands used for the firewire DVD-ROM drive.
To check that your iPod was recognised, you can:

```
$ cat /proc/scsi/scsi
Attached devices:
**Host: scsi0 Channel: 00 Id: 00 Lun: 00
    Vendor: Apple    Model: iPod             Rev: 1.21
    Type:   Direct-Access                    ANSI SCSI revision: 02**

$ cat /proc/partitions
major minor  #blocks  name

    **8     0    9765630 scsi/host0/bus0/target0/lun0/disc
    8     1         31 scsi/host0/bus0/target0/lun0/part1
    8     2      32768 scsi/host0/bus0/target0/lun0/part2
    8     3    9732828 scsi/host0/bus0/target0/lun0/part3**
    3     0   19535040 ide/host0/bus0/target0/lun0/disc
    3     1   12289693 ide/host0/bus0/target0/lun0/part1
    3     2    7028437 ide/host0/bus0/target0/lun0/part2
    3     3     216877 ide/host0/bus0/target0/lun0/part3
```

The parts in **bold** are what you're looking for.
You should also be able to (as root):

```
# dd if=/dev/sda of=/dev/null
```

This can take a while, so press Ctrl+C after a short time.
Note that `fdisk` doesn't recognise Mac partitions, so the `cat /proc/partitions` is to confirm that the disk has been recognised. You should be able to repeat the `dd` command from above for each partition.

## Other Resources

*   [tex9](http://www.tex9.com/) -- iPod software (formerly xtunes) for Linux.
