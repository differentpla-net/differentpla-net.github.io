---
title: "An empeg reports results of OP_STATFID"
date: 2007-12-16T18:59:05.000Z
tags: empeg
---
The next packet that the empeg sends is the actual response to the OP_STATFID request. It looks like this:

```
0000   02 0c 00 07 01 95 00 81 80 00 00 00 00 07 00 00
          ^^^^^                   ''''''''''' ~~~~~~~~
0010   00 0c 00 00 00 7c 7c
       ~~ ___________
```

(Don't worry; I'll stop showing the hex and I'll start talking about higher-level stuff shortly).

This has 12 bytes (`0c 00`) payload. The first 4 (`00 00 00 00`) are a STATUS value. These are similar to Win32's HRESULT values. In this case, zero means that there was no error. The next 4 (`07 00 00 00`) are the FILEID (this is the same as the FILEID we originally asked for). The last 4 bytes (`0c 00 00 00`) are the size (12 bytes).
