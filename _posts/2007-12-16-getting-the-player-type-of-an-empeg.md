---
title: "Getting the player type of an empeg"
date: 2007-12-16T19:37:18.000Z
x-drupal-nid: 205
x-needs-review: 2007-12-16T19:37:18.000Z
tags: empeg
---
The last couple of entries have shown the hex dumps of the individual packets. We'll step back to a slightly higher-level view now.

The first thing that emplode does when it contacts an empeg player is ask it what kind of player it is. Emplode needs to know this so that it can display the correct icon in the discovery window. I'll write up something about how empeg discovery works later.

emplode sends an OP_STATFID request to the player, asking for details about FID_PLAYERTYPE. The player immediately reports progress (as we've seen already, the `string` member of this contains garbage). Shortly afterwards, the player responds with the size of the file, reporting that it's 12 bytes.

emplode then sends an OP_READFID request, asking for the content of FID_PLAYERTYPE. The TransferRequestPacket allows for the PC to ask for the file in chunks. emplode asks for the whole file (chunk_offset=zero, chunk_size=12).

Again, the player immediately reports progress.

Then it responds with the requested data. This is in a TransferResponsePacket. It contains a status (OK), a file_id (garbage), an offset, a byte count and then the data.

The data is a simple ASCIIZ string, containing (in this case) "empeg-car-2". Actually, this is a Rio car, but I don't think the protocol can tell the difference.
