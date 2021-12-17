---
title: "Connecting ChromeOS to a printer via CUPS"
date: 2020-06-29T08:28:09Z
---

I've got my printer connected to a Raspberry Pi print server, and printer discovery only occasionally works from ChromeOS. Here's an aide-memoire for configuring it.

1. In ChromeOS, go to Settings / Printers / Add Printer
2. In the dialog box that appears:

- Name: Whatever
- Address: printserver:631
- Protocol: IPP
- Queue: printers/whatever
