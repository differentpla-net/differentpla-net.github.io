---
title: "Asus A7V Windows Box"
date: 2000-12-08T00:01:00.000Z
tags: hardware pc-builds
---
As mentioned [here](/node/view/3), my Pentium II-400 got fried by an ocuk power supply. As they say, however: Every cloud has a silver lining. With a dead computer, I could finally persuade my girlfriend that some long-overdue upgrades were necessary.

So, I got out my credit card and went shopping...

*   Asus A7V motherboard
*   AMD Athlon T900
*   Adaptec 19160 SCSI card
*   256Mb of RAM
*   Soundblaster Live

The motherboard, CPU and SCSI card I bought from [Scan](http://www.scan.co.uk/). The memory came from [Crucial](http://www.crucial.co.uk/) - they sell Micron memory, which has always proved reliable in the past. The Soundblaster was picked up from [Cambridge Computers](http://www.cambridge-computers.co.uk/).

Items that survived include:

*   IBM UltraStar 36LZX - 18Gb of U160 goodness.
*   Yamaha CRW4416S

## Installing Linux

My primary desktop OS is Windows 2000 (I run Debian 2.2 on my webserver, and my firewall box at home), but I thought that I'd install Linux to give the machine a bit of a workout - if it crashes, Linux is more likely to tell you something useful before falling over.

The first thing I have to say is: "Wow". This thing's fast. I tried compiling g++, and it didn't even touch the sides. The 256Mb of memory ensured that it didn't swap much, if at all.

It was quite an eye-opener compared to the lowly P133 that I run Linux on normally. I'm almost convinced to go out and buy another T900 to replace it.

_Update 2000-12-08:_ [I did]({% post_url 2000-12-08-asus-a7v-linux-box %}).

## Adaptec 19160 and Linux

When I was considering what to buy, I noticed that there was little or no information about whether the 19160 would work under Linux. Adaptec list Linux support for their other cards, but not for this one, which is almost an explicit statement that they don't support it.

While I can't say that it definitely works, it worked correctly for the few tests I tried. Linux identifies it as an AIC-7892.

## Disk corruption

Having said all that, I'm a little suspicious of the adapter now. I've had Windows 2000 installed on this box for a little while now, and I've started getting messages in the Event Log about filesystem corruption, and `chkdsk /f` has had to do nasty sounding things to the filesystem in order to fix it.

I'm not 100% sure that it's down to the SCSI card, but I've got a tech support query in with Adaptec. We'll see what they can suggest.

## Disk corruption update

Finally got a response from Adaptec. They reckon it could be down to the IBM disk supplying SCSI termination power. Apparently, Adaptec cards don't like this. I've not got around to fixing it yet, though. We'll see what happens if I do.

## Another disk corruption update

I checked that the disk wasn't supplying termination power. It's not. This is not good. I'm going to trash the disk completely, and reformat it from the Adaptec SCSI BIOS software. We'll see if that improves matters.
