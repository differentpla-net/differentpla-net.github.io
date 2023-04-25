---
title: "Erlang: Network Protocol Notes"
date: 2023-04-20T13:02:00.000Z
tags: erlang
---

Things to consider:

- Active mode.
- Maybe some basics about binary pattern matching and why it's awesome.
- The need to collect the entire message before attempting to parse it.
  - ferd's post about avoiding catenating the buffer on every packet until you need to. https://cohost.org/mononcqc/post/1128460-on-the-hunt-for-a-bu
- Motivated by: messages larger than the MTU are split.
  - Are there any tools to deliberately introduce splits?
    - If you've got a length prefix, it probably fits in the packet, so you don't need to deal with it being split in two. You _should_.
    - If the sender catenates a bunch of messages, at _that_ point, the size prefix might straddle the packet boundary.
- How to detect size?
  - Length prefix.
  - Terminator, particularly in text-based formats.
  - You know what message it is (based on magic number), so you know how large it is.
- Endianness.
  - Big-endian.
  - Little-endian.
  - Receiver-makes-right.
- Varint encoding.
- Using a separate serde/codec from the network piece.
- Checksums - don't bother for network protocols. The layer below you should be doing that.
- Dealing with errors - just die, or return an error.
- Serialization options.
  - All kinds of stuff to talk about here.
- Correlation IDs.
