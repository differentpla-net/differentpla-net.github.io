---
title: "Partitioning an empeg's Hard Disk Manually"
date: 2004-03-25T09:20:00.000Z
tags: empeg
---
If you're attempting to upgrade the hard disk in your empeg, and the disk builder is not working for you (this is often the case with disks larger than 60Gb), then you might want to build your new disk manually.

Here's how.

Note that if you've got a disk larger than 137Gb, you'll need to install a recent Hijack kernel and use these instructions -- the builder image can only cope with disks smaller than 137Gb.

I'm going to assume that you've got the empeg's disk available as /dev/hdc. This is the case if you're attempting to manually build a second disk in your empeg.

You can also use a 3.5" to 2.5" adapter to connect the new disk to your PC. In this case, I'll assume that you know how to adapt the instructions accordingly.

The first thing we want to do is trash the existing partition table, if any. The simplest way to do that is to zap out the first part of the disk:

```
# dd if=/dev/zero of=/dev/hdc bs=1024 count=1024
```

This writes 1024 blocks, each of 1024 bytes to the start of the disk. It copies the content from /dev/zero, which is a special Linux device that returns an infinitely-sized stream of zeros. It writes these zeros (1Mb of them) to /dev/hdc (your new hard disk).

You can then confirm that you've toasted the partition table:

```
# fdisk -l /dev/hdc
```

(That's a dash-little-ell). It should look something like this:
```
Disk /dev/hdc: 16 heads, 63 sectors, 23392 cylinders
Units = cylinders of 1008 * 512 bytes

Disk /dev/hdc doesn't contain a valid partition table
```

Now to create the partitions required by the empeg. The fdisk program is extremely easy to screw up, so be careful here. Here's a transcript. Note that inputs to `fdisk` should be followed by pressing the Enter key. Where you only need to press the Enter key (to choose the default option), I've highlighted this explicitly:

```
empeg:/empeg/bin# fdisk /dev/hdc
Device contains neither a valid DOS partition table, nor Sun or SGI disklabel
Building a new DOS disklabel. Changes will remain in memory only,
until you decide to write them. After that, of course, the previous
content won't be recoverable.

The number of cylinders for this disk is set to 23392.
There is nothing wrong with that, but this is larger than 1024,
and could in certain setups cause problems with:
1) software that runs at boot time (e.g., LILO)
2) booting and partitioning software from other OSs
   (e.g., DOS FDISK, OS/2 FDISK)

Command (m for help): n
Command action
   e   extended
   p   primary partition (1-4)
e
Partition number (1-4): 1
First cylinder (1-23392, default 1): 1
Last cylinder or +size or +sizeM or +sizeK (1-23392, default 23392): +32M

Command (m for help): n
Command action
   l   logical (5 or over)
   p   primary partition (1-4)
p
Partition number (1-4): 2
First cylinder (67-23392, default 67): (Press Enter)
Using default value 67
Last cylinder or +size or +sizeM or +sizeK (67-23392, default 23392): +32M

Command (m for help): n
Command action
   l   logical (5 or over)
   p   primary partition (1-4)
p
Partition number (1-4): 3
First cylinder (133-23392, default 133): (Press Enter)
Using default value 133
Last cylinder or +size or +sizeM or +sizeK (133-23392, default 23392): +16M

Command (m for help): t
Partition number (1-5): 3
Hex code (type L to list codes): 10
Changed system type of partition 3 to 10 (OPUS)

Command (m for help): n
Command action
   l   logical (5 or over)
   p   primary partition (1-4)
p
Partition number (1-4): 4
First cylinder (166-23392, default 166): (Press Enter)
Using default value 166
Last cylinder or +size or +sizeM or +sizeK (166-23392, default 23392): (Press Enter)
Using default value 23392

Command (m for help): n
First cylinder (1-66, default 1): (Press Enter)
Using default value 1
Last cylinder or +size or +sizeM or +sizeK (1-66, default 66): +16M

Command (m for help): n
First cylinder (34-66, default 34): (Press Enter)
Using default value 34
Last cylinder or +size or +sizeM or +sizeK (34-66, default 66): (Press Enter)
Using default value 66

Command (m for help): t
Partition number (1-6): 6
Hex code (type L to list codes): 82
Changed system type of partition 6 to 82 (Linux swap)

Command (m for help): p

Disk /dev/hdc: 16 heads, 63 sectors, 23392 cylinders
Units = cylinders of 1008 * 512 bytes

   Device Boot    Start       End    Blocks   Id  System
/dev/hdc1             1        66     33232+   5  Extended
/dev/hdc2            67       132     33264   83  Linux
/dev/hdc3           133       165     16632   10  OPUS
/dev/hdc4           166     23392  11706408   83  Linux
/dev/hdc5             1        33     16569   83  Linux
/dev/hdc6            34        66     16600+  82  Linux swap

Command (m for help): w
The partition table has been altered!

Calling ioctl() to re-read partition table.
Re-read table failed with error 16: Device or resource busy.
Reboot your system to ensure the partition table is updated.

WARNING: If you have created or modified any DOS 6.x
partitions, please see the fdisk manual page for additional
information.
Syncing disks.
empeg:/empeg/bin#
```

Basically, what we've done is create an extended partition (/dev/hdc1) containing the 16Mb root partition (/dev/hdc5) and 16Mb swap space (/dev/hdc6). After that is the 32Mb spare partition (/dev/hdc2). The partition after that (using the OPUS partition type) is the dynamic data partition (/dev/hdc3). Then the music partition, which takes up the rest of the disk (/dev/hdc4).

Note that, if you see this:

```
Re-read table failed with error 16: Device or resource busy.
Reboot your system to ensure the partition table is updated.
```

...you'll need to reboot before continuing.
Next: [Formatting an empeg's Hard Disk Manually]({% post_url 2004/2004-03-25-formatting-an-empegs-hard-disk-manually %}).
