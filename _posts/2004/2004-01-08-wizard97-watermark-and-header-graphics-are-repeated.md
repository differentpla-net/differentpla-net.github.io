---
title: "Wizard97: Watermark and Header Graphics are repeated"
date: 2004-01-08T17:14:00.000Z
x-drupal-nid: 113
x-needs-review: 2004-01-08T17:14:00.000Z
tags: mfc
---
I was adding a new wizard to the program that I'm currently working on, so I copied some code from a [Wizard97 demo program](/node/view/214) that I wrote some time ago.

When it comes up, the header and watermark graphics are displayed repeated.

This is shown in the following pictures:

![](/system/files?file=ce6a83801bd16839db16c54b3188c904-27.png)![](/system/files?file=58b05661d2f05bdcc20646c386a1b45d-26.png)
The problem? `PSH_WIZARD97` is defined differently, according to which version of IE you claim to be targetting:

    #if (_WIN32_IE >= 0x0400)
    //----- New flags for wizard97 -----------
    #if (_WIN32_IE < 0x0500)
    #define PSH_WIZARD97            0x00002000
    #else
    #define PSH_WIZARD97            0x01000000
    #endif

Apparently, Microsoft changed the value of the PSH_WIZARD97 flag between IE4 and IE5\. Depending on whether you're running against IE4 or IE5, you should pass a different value.

The simple fix, assuming that you're only supporting IE5 or newer is to add the following to your `StdAfx.h` file:

    #ifndef _WIN32_IE
    #define _WIN32_IE 0x0500	// Allow use of features specific to IE 5.0 or later.
    #endif

![](/system/files?file=ee21e3d40ae5d0ec3a103a3630d9c321-29.png)![](/system/files?file=32c517fe7d65b567370d8289097a7380-28.png)
That's better.

Note that if you claim to only support IE5 or later, you should put runtime checks for this in your code as necessary.
