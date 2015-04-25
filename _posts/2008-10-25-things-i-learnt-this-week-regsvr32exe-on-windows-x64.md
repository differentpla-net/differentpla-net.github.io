---
title: "Things I learnt this week: RegSvr32.EXE on Windows x64"
date: 2008-10-25T16:17:29.000Z
x-drupal-nid: 223
x-needs-review: 2008-10-25T16:17:29.000Z
---
On Windows, 64-bit processes cannot load 32-bit DLLs, and 32-bit processes cannot load 64-bit DLLs. How does REGSVR32.EXE manage to successfully register both 32-bit and 64-bit COM DLLs?

On Windows x64 (I've checked on Windows Vista and Windows 2003, and I assume it's the same for Windows XP and Windows 2008) there are two copies of REGSVR32.EXE. One of them is in C:\Windows\System32, and is 64-bit; the other is in C:\Windows\SysWOW64, and is 32-bit.

If you attempt to use the 64-bit version of REGSVR32.EXE to register a 32-bit DLL, it spots this and spawns the 32-bit version to do the registration. Similarly, if you attempt to use the 32-bit version of REGSVR32.EXE to register a 64-bit DLL, it spawns the 64-bit version.