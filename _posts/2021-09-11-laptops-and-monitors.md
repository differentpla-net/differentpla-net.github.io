---
title: "Multiple Laptops, Multiple Monitors"
date: 2021-09-11T19:04:00Z
---

## Background

At home, I've got a rather nice setup with two Dell U2520D monitors on my desk, which I'm currently using from my (work) Linux desktop. However, more recently, I'm trying to strike a better work/life balance, so I've decided to start making more use of my (personal) Microsoft Surface Book 3 laptop, but I'd like to continue using the two monitors (which are also mine; I never bothered filing the expense claim).

## Surface Book 3

This turns out to work fine. The Dell monitors support DisplayPort pass-through, using DP MST, which means that I can hook everything up like this:

![](/images/2021-09-11-laptops-and-monitors/windows-works.png)

## Macbook Air

But work also gave me a Macbook Air (13", 2019, Intel), so I figure I should make more use of it. When I tried hooking it up like this:

![](/images/2021-09-11-laptops-and-monitors/macos-mirrors.png)

...it didn't work. The Mac treats the two external displays as a single, mirrored, display.

In a conversation on the work Slack, someone pointed out that the Mac would work fine if I connected both monitors via USB-C, like this:

![](/images/2021-09-11-laptops-and-monitors/macos-usb-works.png)

...which works fine, but unfortunately doesn't leave me with any more USB ports on the Mac. I don't want to plug my various peripherals (keyboard, ergonomic mouse, webcam, headset, digital amp, etc.) into the monitor, because that means that the default audio output changes every time the monitor comes out of standby mode, which is really annoying.

## What about a dock?

So I went back to Slack to ask about laptop dock recommendations. The CalDigit TS3 Plus seems to be the most popular answer, but it only has one DisplayPort interface, so in order to support two external monitors, you have to connect one via DP, and one via the dock's USB-C port. To get the USB-connected one to work, you need to install some Displaylink software.

That seemed untidy to me, and CalDigit offer a dock with two DisplayPort interfaces: the CalDigit USB-C Pro Dock, so I ordered one of those instead.

The plan is that it should work like this:

![](/images/2021-09-11-laptops-and-monitors/macos-dock-works.png)

...and it does. For the Mac.

It doesn't work with the Surface Book 3:

![](/images/2021-09-11-laptops-and-monitors/windows-dock-nope.png)

The laptop _detects_ the second monitor just fine. It just refuses to _use_ it.

The Surface Book 3 doesn't have Thunderbolt, which means that (according to the CalDigit website) the dock only supports HD monitors, rather than 2K or 4K monitors.

I kind of skipped over that bit. My thought process went something like this: HD is 1080p, and my monitors are 2560 x 1440, and 1440 isn't _that_ much more than 1080. But, apparently, they're 2K (or QHD), so that's out.

I'm not entirely sure why it doesn't work -- using USB-C without the dock works fine, as explained above, so there's plenty of bandwidth.

This is annoying. The point of the dock is that I should only have to move one cable to switch between laptops.

## Solution

After some thought, I came up with a solution. It's not perfect, and it means that I've paid money for a dock that I'm only half using.

When I'm using the Macbook:

![](/images/2021-09-11-laptops-and-monitors/macos-solution.png)

...and when I'm using the Surface Book:

![](/images/2021-09-11-laptops-and-monitors/windows-solution.png)

It does mean that when I want to switch from Mac to Windows, I need to:

- Switch monitor #1 input to DP.
- Switch monitor #2 input to USB-C.
- Move the USB hub cable from the dock to the Surface Book, if I want to use the peripherals.

...and when I want to switch back to the Mac, I need to:

- Switch monitor #1 input to USB-C.
- Switch monitor #2 input to DP.
- Move the USB hub cable back to the dock.

It's possible that "Auto Input Select" will take care of some of this.

It does mean, however, that -- ignoring the peripherals -- I can leave both laptops connected to both monitors.

## And what about the Linux box?

I can leave that connected to the HDMI inputs on the two monitors. Then the whole thing ends up looking like this:

![](/images/2021-09-11-laptops-and-monitors/everything.png)

...ignore the fact that all three computers seem to be connected to the USB hub at the same time.

To switch to the Linux box, I need to:

- Switch both monitors to HDMI.
- Move the USB hub cable to the Linux box.
