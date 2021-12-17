---
title: Hyper-V Remote Management
date: 2013-02-20T18:59:55Z
---
It turns out that you cannot manage a Windows 2008 R2 Hyper-V installation from
a Windows 8 desktop (even with the Remote Server Administration Tools for
Windows 8 installed) or from Windows 2012 Server.

You *can*, however, manage a Windows 2012 Hyper-V installation from Windows
Server 2008 R2. While I've not tried it, I suspect that this means that you can
do the same from Windows 7 RSAT.

This means that people with an estate containing mixed servers won't be able to
upgrade their management desktops to Windows 8.

With a bit of luck, Microsoft might notice this oversight and fix it soon.
