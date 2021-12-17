---
title: "Hyper-V: creating a new Virtual Hard Disk fails with \"Cannot create a file when that file already exists\""
date: 2009-10-21T08:12:36.000Z
x-drupal-nid: 241
x-needs-review: 2009-10-21T08:12:36.000Z
---
My desktop PC is Windows 2008, with Hyper-V disabled. I've got a Windows 2008 R2 box to act as a Hyper-V server. I created an administrator user account with my name on the R2 box, and used [HVRemote](http://code.msdn.microsoft.com/HVRemote) to allow remote access.

All good until I attempted to create a VHD file. Then I got "cannot create a file when that file already exists". Huh? The file definitely doesn't exist. What's going on?

UAC is what's going on.

Disable UAC (well, turn it all the way down, given that this is R2) **and reboot the box**.

But why the odd error message? Time for Process Monitor, but in short: the Hyper-V service attempts to open the `C:\Users\Public\Documents\Hyper-V\Virtual hard disks` directory and gets an Access Denied error (because of UAC). It misinterprets this, and attempts to create the directory. This gives the "file already exists" error, which is the one that's reported.
