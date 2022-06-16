---
title: "empeg files"
date: 2002-03-07T12:21:00.000Z
tags: empeg
---

Some stripped down utilities for the empeg:

- [base.tar.gz](/files/2002/2002-03-07-empeg-files/base.tar.gz): binaries, libraries, etc. to get `ping`, `rsync`, etc. working.
- [configs.tar.gz](/files/2002/2002-03-07-empeg-files/configs.tar.gz): my configuration files; use them as examples.

To install them, use ZModem transfer in minicom:

1. Press Ctrl+C to quit the player.
2. Re-mount the disks read/write: `rw ; rwm`
3. `cd /drive0`
4. In minicom, `Ctrl+A`, `S`, select `zmodem`.
5. Browse to the file to send: To enter a directory, press `Space` twice. To select a file, press `Space` once. Press
   `Enter` to start the upload.
6. `cd /`
7. `tar xfz /drive0/base.tar.gz`, for example.
8. Re-mount the disks read-only: `ro ; rom`. This might not work (`mount: / is busy`); in that case, press `Ctrl+D` to
   return to the player and then `Ctrl+C` to quit the player. The player will re-mount the disks read-only when it
   quits.
