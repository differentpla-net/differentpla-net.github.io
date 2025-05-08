---
title: "Packet format"
date: 2007-12-16T18:18:47.000Z
tags: empeg
---
The format of the packets in the empeg's TCP/IP protocol owe a lot to the fact that the empeg mk1 communicated over a serial link. This means that there's a certain amount of cruft still in the protocol.

Most of this information is in the `emptool` sources, but I'm going to explain some of it in more detail here.

As revealed by Wireshark, the first thing that emplode asks for is the player's type. Let's take a quick look at the request packet:

<pre>02 04 00 07 00 95 00 81 80 07 00 00 00 FC 43</pre>

This breaks down into a header, payload and trailer:

## Header

<pre><u>02 04 00 07 00 95 00 81 80</u> 07 00 00 00 FC 43</pre>

All packets start with 0x02\. The emptool source code calls this PSOH. This is left over from the serial protocol. It's a pseudo-SOH (start-of-header). It signals the start of the packet.

The next two bytes (`04 00`) are the size of the payload. Note that the empeg protocol is little-endian. This is because the x86 is little-endian, and Linux on the empeg uses the ARM in little-endian mode. It would have been wasteful to convert to network byte order (big-endian) and back again, so everything's in little-endian.

With that in mind, we can see that the payload is 4 bytes.

The next byte (`07`) is the opcode. 0x07 is OP_STATFID, which is used to discover the size of a file on the empeg, so that we can allocate enough memory, and so that we can report progress.

The next byte (`00`) is the optype. 0x00 is OPTYPE_REQUEST, so this is a request packet.

The last 4 bytes (`95 00 81 80`) are the packet ID. I'll talk about packet IDs in the future.

## Payload

<pre>02 04 00 07 00 95 00 81 80 <u>07 00 00 00</u> FC 43</pre>

We know from looking at the header that the payload size is 4 bytes. It's little endian, so it's a seven.

## Trailer

<pre>02 04 00 07 00 95 00 81 80 07 00 00 00 <u>FC 43</u></pre>

The trailer is the CRC16 of the packet, starting with the opcode (i.e. opcode, optype, packet id and payload).
