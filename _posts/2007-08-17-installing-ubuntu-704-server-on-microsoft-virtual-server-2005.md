---
title: "Installing Ubuntu 7.04 Server on Microsoft Virtual Server 2005"
date: 2007-08-17T12:04:43.000Z
x-drupal-nid: 193
x-needs-review: 2007-08-17T12:04:43.000Z
---
## Installation

When attempting to install Ubuntu on Microsoft Virtual Server (from ubuntu-7.04-server-i386.iso), the installation screen is stretched vertically, but the VMRC window isn't. This means that you can only see the top half of the display.

To fix it, at the Ubuntu installation splash screen (the one where it says "Install to the hard disk"), press F6 (Other Options) and type `**vga=0x314**`. Then press Enter. This boot option causes Ubuntu to use an alternative video mode that works correctly in Virtual Server.

There doesn't appear to be any need to edit your GRUB configuration to make this permanent. I assume because this is ubuntu-server.
