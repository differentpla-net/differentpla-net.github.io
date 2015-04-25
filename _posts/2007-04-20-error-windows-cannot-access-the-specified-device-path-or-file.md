---
title: "Error: Windows cannot access the specified device, path or file."
date: 2007-04-20T10:38:13.000Z
x-drupal-nid: 164
x-needs-review: 2007-04-20T10:38:13.000Z
---
I started getting this error on one of my Windows 2003 boxes yesterday:

<tt>Windows cannot access the specified device, path or file. You may not have the appropriate permissions to access the item.</tt>

This would happen whenever I double-clicked on an EXE file in Windows Explorer.

It started occurring after I installed SQL Server 2005, although I'm not sure this had much to do with it.

I tried using Google to search for a solution; most of the suggestions talked about disabling anti-virus or anti-spyware software. I don't have any installed on this box -- it's virtual, and it's not attached to any networks.

Anyway, one of the suggestions was to **disable Internet Explorer Enhanced Security**. This is under Add/Remove Windows components.

I tried this, and the machine started working again. Hurrah.