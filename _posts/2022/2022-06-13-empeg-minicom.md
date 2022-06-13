---
title: "Using minicom with an empeg"
date: 2022-06-13T13:38:00Z
tags: empeg minicom
---

It's been many years since I last connected my empeg player to a Linux PC, and the muscle memory's gone. Here's how to
make it work with Ubuntu 20.04.

## Install minicom

```
sudo apt install minicom
```

## Configure udev

I'm using a "Magic Control Technology Corp. MCT-232 Serial Port" USB-to-serial converter, with a null-modem cable. When
connected to the Linux PC, it appears as `/dev/ttyUSB0`, but the permissions don't allow access. We can configure udev
to fix this up.

Based on my answer about `adb` permissions [here](https://stackoverflow.com/questions/14460656/android-debug-bridge-adb-device-no-permissions/19291975#19291975), I went with the following:

First, we need the USB VID and PID for the adapter:

```
$ lsusb
Bus 001 Device 004: ID 0711:0230 Magic Control Technology Corp. MCT-232 Serial Port
```

Then we create `/etc/udev/rules.d/80-mct232.rules`:

```
ATTRS{idVendor}=="0711", ATTRS{idProduct}=="0230", MODE="0664", GROUP="plugdev"
```

Restart udev:

```
$ sudo udevadm control --reload-rules
$ sudo service udev restart
```

Unplug/replug your device. It should appear as `/dev/ttyUSB0` owned by the `plugdev` group.

## Run minicom

The following will sort of work:

```
minicom -D /dev/ttyUSB0 -b 115200
```

But we'll need to tweak various other settings, so, following <https://www.centennialsoftwaresolutions.com/post/configure-minicom-for-a-usb-to-serial-converter>:

1. Press `Ctrl+A`, `O` (upper-case O-for-Oscar) to bring up the configuration.
2. Select "Serial port setup".
3. Serial device: `/dev/ttyUSB0`.
4. Bps/Par/Bits: `115200 8N1`.
5. Hardware flow control: `No`.
6. Software flow control: `No`.
7. Press Enter to return to the configuration menu.
8. Choose "Save setup as dfl".

Or just create `~/.minirc.dfl` as follows:

```
# Machine-generated file - use setup menu in minicom to change parameters.
pu port             /dev/ttyUSB0
pu baudrate         115200
pu bits             8
pu parity           N
pu stopbits         1
pu rtscts           No
pu xonxoff          No
```

(Yes, I know it's not actually machine-generated...)

Then you can just run `minicom`.

## Interacting with the device

By default, the player doesn't output anything to the serial port, so it'll look like nothing's happening. If you
power-cycle it, you should see the boot messages. If you press Ctrl+C, you should be dropped to the bash prompt. Exiting
from bash should restart the player.

For a list of serial port commands, see [this page]({% post_url 2003/2003-04-02-empeg-serial-port-commands %}).

## Windows?

I initially attempted to use the converter with Windows, but there are apparently no drivers for Windows 11, so that's a
non-starter.
