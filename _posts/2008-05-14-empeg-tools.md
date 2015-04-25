---
title: "empeg tools"
date: 2008-05-14T06:28:15.000Z
x-drupal-nid: 216
x-needs-review: 2008-05-14T06:28:15.000Z
---
In response to [this thread](http://empegbbs.com/ubbthreads.php/ubb/showflat/Number/306574) on the empeg BBS, I hacked together a couple of small tools for the empeg car player.

They're not particularly well-integrated yet, but that'll come later.

They're command-line programs, so you'll need to open a console window before running them. They all require .NET 2.0, which is installed by default on Windows Vista, but you might need to download and install it on other OS versions and editions. They also work fine on Mono.

## FindEmpeg

This program looks for empegs on the local network. Run it with no arguments, as follows:

<pre>C:\Temp>FindEmpeg.exe
crowley at 10.0.0.7
crowley at 10.0.0.7
^C</pre>

It'll scan the local network once every thirty seconds, displaying the empeg's name and its IP address. Press Ctrl+C to stop it.

Note that the Windows Vista firewall might prompt you to unblock the application. It seems to still work OK even if you don't.

## EmpegGet

Given the IP address of the empeg found above, this downloads the database files from the empeg. They're placed into the current directory (this will be fixed later). Run it as follows:

<pre>C:\Temp>EmpegGet.exe 10.0.0.7
Getting empeg database from 10.0.0.7
Copying http://10.0.0.7/empeg/var/database3 to ./database3
Copying http://10.0.0.7/empeg/var/tags to ./tags
Copying http://10.0.0.7/empeg/var/playlists to ./playlists</pre>

It requires the following:

*   Your empeg must be running v3.0-alpha.
*   Your empeg must be running the Hijack kernel.
*   The HTTP server must be enabled in the Hijack kernel.

You can see if it succeeded, because the database files are placed in the current directory:

*   database3
*   playlists
*   tags

## EmpegDump

This parses the database files downloaded above and dumps them, in depth-first order, to the screen. I'll add XML, HTML or XPS output in a later release:

<pre>C:\Temp>EmpegDump.exe
00000100: empeg-car
 00000120: Artists
  00000130: 1000 Clowns
   00000140: (Not the) Greatest Rapper
    00000150: (Not the) Greatest Rapper (LP Version)
    00000160: (Not the) Greatest Rapper (Instrumental)
    00000170: Rainy Days (LP Version)
  000183f0: 2 Many DJ's
   00018400: As Heard On Radio Soulwax Pt. 2
    00018410: Peter Gunn (Live) & Where's Your Head At (Head-A-Pella)
(etc.)</pre>

Each line shows the FID in hex, followed by the title, indented under the parent playlist.