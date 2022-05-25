---
title: "Installing Windows XP Pro on a Sony Vaio SRX87"
date: 2004-01-02T16:52:00.000Z
tags: sony-vaio windows-xp
---
## Introduction

I've recently acquired a shiny new Hitachi 7K60 hard disk to go into my laptop. The disk is 60Gb, which will hopefully
give me enough space to dual-boot Windows XP and Linux. I ran out of space on the 20Gb disk that the Sony comes with,
and I had to remove Linux. The other nice thing about this disk is that it's a 7200 rpm disk, which should make those
away-from-home hacking sessions go much more smoothly.

Unfortunately, while the Sony Vaio comes with Windows XP, it only comes in the form of a "System Recovery CD" which, in
addition to installing any amount of junk (e. g. "Everquest"), only comes with support for the hard disk sizes
originally shipped in these laptops.

This article will explain how I went about reinstalling a full copy of Windows XP Professional on this laptop.

Note that the SRX87 comes with a copy of Windows XP Home, while the SRX87P comes with a copy of Windows XP Professional.

Fortunately, I've got two copies of Windows XP Professional that I was given the last time that I visited Microsoft's
Redmond campus.

## Windows XP Setup Disk

When I last reinstalled Windows XP on my main desktop PC, I created a "slipstreamed" installation CD. This installs
Windows XP already patched with Service Pack 1. See [this page](http://www.windows-help.net/WindowsXP/winxp-sp1-bootcd.html) for more information.

I needed to do this for my desktop PC because it has a 250Gb hard disk, and the original setup disk for Windows XP
doesn't support disks this large.

I made sure to use the serial number from my second copy of Windows XP, however.

## Extracting Application Recovery CD

The Sony comes with an "Application Recovery CD", allowing you to separately reinstall all of the junk that comes on the
"System Recovery CD". This also comes with some really annoying front end software. You can't get around this software,
because everything on the disk is in a single file called `SONY.PAC`. Fortunately, I found some instructions on
unpacking these files:

- <http://pub173.ezboard.com/funofficialsonyfrm26.showMessage?topicID=27.topic>
- <http://www.myplc.com/sony/sony_apps.htm>

To make life easier in future, I created a DVD containing the individual applications from the "Application Recovery
CD"; I renamed Sony's cryptic directories to something more sensible. I also put the relevant downloaded drivers
(from Sony support) and some other applications and utilities on the DVD.

## Partitioning the Hard Disk

Because I'm planning to install Linux on this laptop as well, I needed to partition the hard disk to allow room. This is
easily done from inside the Windows XP setup program. I opted for a 36Gb partition for Windows, leaving the rest for
Linux.

## Configuring the Network

By default, Windows XP doesn't come with drivers for the network adapter in the Vaio, so I selected "Custom settings"
when prompted to configure network settings. The first adapter found was the Orinoco Wireless card, for which I turned
off File and Printer sharing.

I was then asked to select a workgroup. Since this laptop is primarily being used at home, I went with the default
workgroup name "WORKGROUP".

## Installing Network Drivers

Since Windows XP doesn't come with support for the Intel network card in the Vaio, it's necessary to install the
drivers. These are (now) on my groovy recovery DVD, which makes life easier. Unfortunately, they require a reboot
afterwards.

## Installing Other Drivers

While we're in the process of installing drivers, we might as well install the others:

- ALPS touchpad driver; reboot.
- LCD driver; reboot.
- Modem driver; reboot.
- Orinoco wireless driver; reboot.
- Sony Cardbus driver (as distinct from the 16-bit Cardbus driver, see below); reboot.
- Video driver; reboot.
- Audio driver; reboot.

It's entirely possible that some of these don't actually need the reboot. It's better to be safe than sorry, though.

There are a couple of things on the Sony support site that I didn't install:

- The 16-bit Cardbus drivers. The last time I reinstalled Windows on this laptop, they didn't install. I don't remember
  why.
- The BIOS update. It requires a floppy disk - presumably it creates a bootable DOS floppy with the BIOS updater on it.
  I don't have a floppy disk drive for this laptop.
- SonicStage 1. 2 Update. I haven't installed SonicStage, and don't plan to. Installing the update would be pointless.
- Sony DVgate Update Utility. Again, I haven't installed DVgate and don't plan to.
- Sony USB mouse driver. I don't have a USB mouse. Well, actually I do, but I don't plan on using it with the laptop.
- Vaio clock screensaver update. I haven't installed it, I don't plan to.

It's worth noting that this collection of updates were downloaded from the Sony support site a while ago. There may be newer versions on there now. It's worth checking.

- <http://www.ita.sel.sony.com/support/>

## Installing Applications

It's time to install other applications from the Application Recovery CD. Obviously, I'll actually be using the DVD I created earlier.

- Sony Shared Library; reboot.
- Sony Utilities Library. Some of the other applications require these, so we'll install them first. This one doesn't
  ask for a reboot.
- Hotkey utility. This is what makes the Fn key work. It doesn't ask for a reboot. It appears to need a reboot before
  it'll run, though.
- Jogdial utility. The SRX87 has a small jogdial attached to the touchpad. This is what makes it work. Reboot. The
  jogdial utility also needs configuring: "Show Jog Dial only when in use".
- JogGUI Plugin; reboot.
- Notebook Setup. Doesn't ask for a reboot.
- PowerPanel. Doesn't ask for a reboot.
- UI Designer Selector. Doesn't ask for a reboot.

And that's it. I don't need most of the other junk.

## DVD Playback Software

The Sony comes with a DVD drive, so it'd be nice to be able to play back DVD movies occasionally. By default, Windows
Media Player 9 doesn't include DVD playback -- you can install it as an add-on. Instead, I installed WinDVD 2000 off of
the Application Recovery CD. It doesn't enable DVD playback for WMP9, but it does allow watching DVDs on the laptop.

At some point, I'll be looking at a DVD->DivX solution, so that I don't need to take the DVD drive with me when I go
travelling.
