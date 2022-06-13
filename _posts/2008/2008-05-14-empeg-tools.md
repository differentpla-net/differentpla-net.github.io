---
title: "empeg tools"
date: 2008-05-14T06:28:15.000Z
tags: empeg
---
In response to [this thread](http://empegbbs.com/ubbthreads.php/ubb/showflat/Number/306574) on the empeg BBS, I hacked together a couple of small tools for the empeg car player.

They're not particularly well-integrated, and (14 years later) I didn't get around to fixing that.

They're command-line programs, so you'll need to open a console window before running them. They all require .NET 2.0, which is installed by default on Windows Vista, but you might need to download and install it on other OS versions and editions. They also work fine on Mono.

## FindEmpeg

This program looks for empegs on the local network. Run it with no arguments, as follows:

```
C:\Temp>FindEmpeg.exe
crowley at 10.0.0.7
crowley at 10.0.0.7
^C
```

It'll scan the local network once every thirty seconds, displaying the empeg's name and its IP address. Press Ctrl+C to stop it.

Note that the Windows firewall might prompt you to unblock the application. It seems to still work OK even if you don't.

## EmpegGet

Given the IP address of the empeg found above, this downloads the database files from the empeg. They're placed into the current directory. Run it as follows:

```
C:\Temp>EmpegGet.exe 10.0.0.7
Getting empeg database from 10.0.0.7
Copying http://10.0.0.7/empeg/var/database3 to ./database3
Copying http://10.0.0.7/empeg/var/tags to ./tags
Copying http://10.0.0.7/empeg/var/playlists to ./playlists
```

It requires the following:

*   Your empeg must be running v3.0-alpha.
*   Your empeg must be running the Hijack kernel.
*   The HTTP server must be enabled in the Hijack kernel.

You can see if it succeeded, because the database files are placed in the current directory:

*   `database3`
*   `playlists`
*   `tags`

## EmpegDump

This parses the database files downloaded above and dumps them, in depth-first order, to the screen:

```
C:\Temp>EmpegDump.exe
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
(etc.)
```

Each line shows the FID in hex, followed by the title, indented under the parent playlist.

<div class="callout callout-dark" markdown="span">
The files are attached: [empeg-tools.zip](/files/2008/2008-05-14-empeg-tools/empeg-tools.zip). `EmpegGet` and `EmpegDump` work on Windows 11 (provided you install .NET 3.5 when prompted), but `FindEmpeg` appears to be broken.
</div>
