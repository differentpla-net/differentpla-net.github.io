---
title: "Shell Fish : For Fishing in your Windows Shell"
date: 2004-01-12T11:55:00.000Z
x-drupal-nid: 119
x-needs-review: 2004-01-12T11:55:00.000Z
---
Provides a quick way to find out where Windows is putting your "Favorites", "Application Data" etc. folders.

![](/system/files?file=5fbc0bfcaee75dfb7834d806335d75e6-51.png)

Note that this also demonstrates:

*   Using SHGetFileInformation to get hold of the system image list for using in a ListView control.
     Note that if you do this, you should make sure that your list control has the LVS_SHAREIMAGELISTS style. If you don't, you'll get odd behaviour in Windows 98 when you exit the application.
*   How to use CStatusBar to make a resizable dialog. You'll still need to reposition the controls yourself, but the grabber is drawn for you.
