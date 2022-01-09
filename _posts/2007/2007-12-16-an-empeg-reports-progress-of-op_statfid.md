---
title: "An empeg reports progress of OP_STATFID"
date: 2007-12-16T18:53:10.000Z
tags: empeg
---
We've seen the format of the packet that emplode sends to find out the length of a file on the empeg (OP_STATFID). The empeg seems to always send a progress packet in response. The packet looks like this:

<pre>0000   02 54 00 07 02 95 00 81 80 3c 00 00 00 00 00 00
0010   00 00 00 00 00 00 00 00 00 01 00 00 00 34 d8 11
0020   02 10 01 12 02 96 00 81 80 3c e9 1b 02 58 fc df
0030   bd 40 fc df bd ec fe 0d 02 d0 d7 11 02 18 41 1d
0040   02 88 fc df bd 48 01 23 02 6c fc df bd 5c fc df
0050   bd 18 ff 0d 02 88 fc df bd 48 01 23 02 38 87</pre>

We already know how to parse the header, so we can see that the payload is 84 bytes long, that this is an OP_STATFID packet, and that it's a a progress packet (0x02 = OPTYPE_PROGRESS).

The emptool sources tell us how to parse the rest of the packet. `3c 00 00 00` is a new timeout value. It's in seconds, so the empeg is telling us to hold on for another minute. The next 4 numbers are the progress values. A lot of the empeg source code assumes that things are broken down into stages, and that we can report progress on each stage. In this case, the numbers don't really mean much.

What's puzzling me, though, is that the `string` member of the response packet appears to be total garbage. The name implies that it's a human-readable string returned from the player that can be displayed by emplode. It's passed to the protocol observer (if any). emptool doesn't provide one, and neither does (as far as I remember) emplode.

This is probably a good thing, because it appears to be garbage. It's probably old stack frames or something similar.
