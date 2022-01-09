---
title: "Adding custom logon pictures to Windows XP"
date: 2004-04-02T12:25:00.000Z
---
Inspired by [this post](http://blogs.msdn.com/oldnewthing/archive/2004/04/01/105582.aspx) on Raymond Chen's [blog](http://blogs.msdn.com/oldnewthing/), I thought I might explain how to change the logon pictures.

The pictures are stored in `C:\Documents and Settings\All Users\Application Data\Microsoft\User Account Pictures\Default Pictures`. To add your own, just create a 48x48 .BMP file and drop it into this directory. It'll then become available in the "Pick a new picture for your account" dialog.

Logon pictures in use are in `C:\Documents and Settings\All Users\Application Data\Microsoft\User Account Pictures`, stored as _username_.bmp.

If you use the "Browse for more pictures" to select another picture, it's converted to the correct size and format and then dropped into this directory.