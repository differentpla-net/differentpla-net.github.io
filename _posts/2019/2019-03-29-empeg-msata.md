---
title: "Installing an mSATA disk in an empeg"
date: 2019-03-29T15:25:00.000Z
tags: empeg
---

## Background

The hard disk in my spare empeg player ("toothgnip") died at some point. I'm using a 64GiB Transcend 2.5" PATA SSDs in
my main empeg player ("crowley"), but they're getting harder and harder to find, so I opted for a Kingston mSATA disk
(240G) and a JMicron-based mSATA to PATA adapter.

But that left me with a problem: I had no way to install the player software. Ordinarily, you'd download the `.upgrade` file and apply it over a serial port with _empegUpgrade_.

But I don't have a suitable Windows PC any more. I have a Surface Go, running Windows 10. It only just runs _emplode_ correctly. I think my odds of getting _empegUpgrade_ to work on Windows 10, with a USB-serial adapter are slim-to-none.

So: how to get the player software installed?

_/me strokes chin_

I _do_ have a Linux desktop PC, so I bought an mSATA-to-USB 3 adapter, and connected the disk using that.

## Partitioning the disk

Connect disk to Linux PC. It appears as `/dev/sdd`.

Partition it:

```
roger@roger-pc:~ $ sudo fdisk /dev/sdd

Welcome to fdisk (util-linux 2.27.1).
Changes will remain in memory only, until you decide to write them.
Be careful before using the write command.

Device does not contain a recognised partition table.
Created a new DOS disklabel with disk identifier 0x7b3200ef.

Command (m for help): n
Partition type
   p   primary (0 primary, 0 extended, 4 free)
   e   extended (container for logical partitions)
Select (default p): e
Partition number (1-4, default 1): 1
First sector (2048-468862127, default 2048): <Press Enter>
Last sector, +sectors or +size{K,M,G,T,P} (2048-468862127, default 468862127): +32M

Created a new partition 1 of type 'Extended' and of size 32 MiB.

Command (m for help): n
Partition type
   p   primary (0 primary, 1 extended, 3 free)
   l   logical (numbered from 5)
Select (default p): p
Partition number (2-4, default 2): 2
First sector (67584-468862127, default 67584): <Press Enter>
Last sector, +sectors or +size{K,M,G,T,P} (67584-468862127, default 468862127): +32M

Created a new partition 2 of type 'Linux' and of size 32 MiB.

Command (m for help): n
Partition type
   p   primary (1 primary, 1 extended, 2 free)
   l   logical (numbered from 5)
Select (default p): p
Partition number (3,4, default 3): 3
First sector (133120-468862127, default 133120): <Press Enter>
Last sector, +sectors or +size{K,M,G,T,P} (133120-468862127, default 468862127): +16M

Created a new partition 3 of type 'Linux' and of size 16 MiB.

Command (m for help): t
Partition number (1-6, default 6): 3
Partition type (type L to list all types): 10

Changed type of partition 'Linux' to 'OPUS'.

Command (m for help): n
Partition type
   p   primary (2 primary, 1 extended, 1 free)
   l   logical (numbered from 5)
Select (default p): p

Selected partition 4
First sector (165888-468862127, default 165888): <Press Enter>
Last sector, +sectors or +size{K,M,G,T,P} (165888-468862127, default 468862127): <Press Enter>

Created a new partition 4 of type 'Linux' and of size 223.5 GiB.

Command (m for help): n
All primary partitions are in use.
Adding logical partition 5
First sector (4096-67583, default 4096): <Press Enter>
Last sector, +sectors or +size{K,M,G,T,P} (4096-67583, default 67583): +16M

Created a new partition 5 of type 'Linux' and of size 16 MiB.

Command (m for help): n
All primary partitions are in use.
Adding logical partition 6
First sector (38912-67583, default 38912): <Press Enter>
Last sector, +sectors or +size{K,M,G,T,P} (38912-67583, default 67583): <Press Enter>

Created a new partition 6 of type 'Linux' and of size 14 MiB.

Command (m for help): t
Partition number (1-6, default 6): 6
Partition type (type L to list all types): 82

Changed type of partition 'OPUS' to 'Linux swap / Solaris'.

Command (m for help): p
Disk /dev/sdd: 223.6 GiB, 240057409536 bytes, 468862128 sectors
Units: sectors of 1 * 512 = 512 bytes
Sector size (logical/physical): 512 bytes / 4096 bytes
I/O size (minimum/optimal): 4096 bytes / 33553920 bytes
Disklabel type: dos
Disk identifier: 0x7b3200ef

Device     Boot  Start       End   Sectors   Size Id Type
/dev/sdd1         2048     67583     65536    32M  5 Extended
/dev/sdd2        67584    133119     65536    32M 83 Linux
/dev/sdd3       133120    165887     32768    16M 10 OPUS
/dev/sdd4       165888 468862127 468696240 223.5G 83 Linux
/dev/sdd5         4096     36863     32768    16M 83 Linux
/dev/sdd6        38912     67583     28672    14M 82 Linux swap / Solaris

Partition table entries are not in disk order.

Command (m for help): w
The partition table has been altered.
Calling ioctl() to re-read partition table.
Synching disks.
```

## Create swap

```
roger@roger-pc:~ $ sudo mkswap /dev/sdd6
Setting up swapspace version 1, size = 14 MiB (14675968 bytes)
no label, UUID=87268e95-7576-449a-b997-ca1e233b82ea
```

## Wipe dynamic data

```
roger@roger-pc:~ $ sudo dd if=/dev/zero of=/dev/sdd3
dd: writing to '/dev/sdd3': No space left on device
32769+0 records in
32768+0 records out
16777216 bytes (17 MB, 16 MiB) copied, 0.685017 s, 24.5 MB/s
```

## Format the music partition

```
roger@roger-pc:~ $ sudo mkfs.ext2 -t ext2 -v -b4096 -s 1 -i 262144 -m 0 -I 128 /dev/sdd4
mke2fs 1.42.13 (17-May-2015)
fs_types for mke2fs.conf resolution: 'ext2'
Filesystem label=
OS type: Linux
Block size=4096 (log=2)
Fragment size=4096 (log=2)
Stride=0 blocks, Stripe width=8191 blocks
915456 inodes, 58587030 blocks
0 blocks (0.00%) reserved for the super user
First data block=0
Maximum filesystem blocks=4294967296
1788 block groups
32768 blocks per group, 32768 fragments per group
512 inodes per group
Filesystem UUID: 32a19196-f352-4573-baf1-088e995d7ac8
Superblock backups stored on blocks:
	32768, 98304, 163840, 229376, 294912, 819200, 884736, 1605632, 2654208,
	4096000, 7962624, 11239424, 20480000, 23887872

Allocating group tables: done
Writing inode tables: done
Writing superblocks and filesystem accounting information: done
```

```
roger@roger-pc:~ $ sudo tune2fs -c -1 -i0 /dev/sdd4
tune2fs 1.42.13 (17-May-2015)
Setting maximal mount count to -1
Setting interval between checks to 0 seconds
```

```
roger@roger-pc:~ $ sudo mount /dev/sdd4 /mnt
roger@roger-pc:~ $ sudo mkdir -p /mnt/fids
roger@roger-pc:~ $ sudo mkdir -p /mnt/var
roger@roger-pc:~ $ sudo sh -c "echo '[hijack]' > /mnt/var/config.ini"
```

```
roger@roger-pc:~ $ sudo umount /mnt
```

## Extract the `.upgrade` file parts

With Mark Lord's `upgrader` and `car2_v3a11_hijack.upgrade`:

```
roger@roger-pc:~ $ ./upgrader --extract car2_v3a11_hijack.upgrade

/upgrader: Empeg/RioCar Ethernet Upgrade Tool, version 0.95, (c)2003 by Mark Lord

...etc.
```

## Write the player software

```
roger@roger-pc:~ $ sudo dd if=_dev_hda5 of=/dev/sdd5
32768+0 records in
32768+0 records out
16777216 bytes (17 MB, 16 MiB) copied, 0.699272 s, 24.0 MB/s
```

With Mark Lord's `download.c`:

```
roger@roger-pc:~ $ ./download _proc_flash_0c000 0c000 /dev/ttyUSB1
Turn on empeg unit now
Found empeg unit: entering program mode
Manufacturer=0089, product=88c1
Starting erase [1100%] erase ok
Starting program at 0xc000 [100%] program ok
```

```
roger@roger-pc:~ $ ./download _proc_flash_0e000 0e000 /dev/ttyUSB1
Turn on empeg unit now
Found empeg unit: entering program mode
Manufacturer=0089, product=88c1
Starting erase [100%] erase ok
Starting program at 0xe000 [100%] program ok
```

```
roger@roger-pc:~ $ ./download _proc_flash_b0000 b0000 /dev/ttyUSB1
Turn on empeg unit now
Found empeg unit: entering program mode
Manufacturer=0089, product=88c1
Starting erase [100%] erase ok
Starting program at 0xb0000 [100%] program ok
```

```
roger@roger-pc:~ $ ./download _proc_empeg_kernel 10000 /dev/ttyUSB1
Turn on empeg unit now
Found empeg unit: entering program mode
Manufacturer=0089, product=88c1
Starting erase [100%] erase ok
Starting program at 0x10000 [100%] program ok
```

## Put the disk in the empeg

...and fire up _emplode_:

> Est. Free Space: 223GB

Job done.
