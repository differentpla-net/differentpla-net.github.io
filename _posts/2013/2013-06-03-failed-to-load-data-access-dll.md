---
title: Failed to load data access DLL
date: 2013-06-03T10:55:33Z
---

While attempting to debug a crash dump from a .NET 4.0 RTM process on my (.NET 4.5), I got the dreaded
`Failed to load data access DLL, 0x80004005` error message.

**tl;dr? Skip to the end...**

In full, the error looks like this:

```
0:000> .loadby sos clr
0:000> !vmstat
Failed to load data access DLL, 0x80004005
Verify that 1) you have a recent build of the debugger (6.2.14 or newer)
            2) the file mscordacwks.dll that matches your version of clr.dll is
                in the version directory or on the symbol path
            3) or, if you are debugging a dump file, verify that the file
                mscordacwks_<arch>_<arch>_<version>.dll is on your symbol path.
            4) you are debugging on supported cross platform architecture as
                the dump file. For example, an ARM dump file must be debugged
                on an X86 or an ARM machine; an AMD64 dump file must be
                debugged on an AMD64 machine.

You can also run the debugger command .cordll to control the debugger's
load of mscordacwks.dll.  .cordll -ve -u -l will do a verbose reload.
If that succeeds, the SOS command should work on retry.

If you are debugging a minidump, you need to make sure that your executable
path is pointing to clr.dll as well.
```

There are lots of words here, but they're not the easiest to understand,
particularly when you're in a hurry (like, say, you're attempting to debug an
issue on a production server).

I'm assuming that you've got a recent version of WinDbg. I was running 6.12, which should be OK.

If you're lucky, however, (and you'll need to have `!sym noisy` turned on to see this), you'll get:

```
DBGHELP: clr - public symbols
            c:\websymbols\clr.pdb\0A821B8A573E42899202851DF6A539F12\clr.pdb
SYMSRV:  mscordacwks_AMD64_AMD64_4.0.30319.01.dll from http://msdl.microsoft.com/download/symbols: 502605 bytes - copied
DBGHELP: c:\websymbols\mscordacwks_AMD64_AMD64_4.0.30319.01.dll\4BA21EEB965000\mscordacwks_AMD64_AMD64_4.0.30319.01.dll - OK
```

This means that the Microsoft Symbol Server had the relevant `mscordacwks.dll`
file. If not, you'll have to copy it from the machine from where the crash dump
originates, and put it somewhere useful. Unfortunately, the message doesn't
explain what, exactly, it means by "on your symbol path".

For me, at least, it seems that putting it in (e.g.) `C:\WebSymbols` doesn't
work -- you need to put it in the correct named location. I can't verify this
right now, since it downloaded it successfully from Microsoft Symbol Server.

More importantly for my problem, however, is the easily-overlooked message at
the end:

```
If you are debugging a minidump, you need to make sure that your executable
path is pointing to clr.dll as well.
```
Given that WinDbg *knows* that I'm debugging a minidump, it could have made
this more prominent. What it's telling us is this:

1. Copy clr.dll from the original machine. Put it somewhere. Next to the dump file would be fine.
2. Add that path to the executable search path, using `.exepath+ C:\wherever`

Note, that you **must** use the version from the original machine. If you
attempt to use your `clr.dll`, you'll get the message:

```
The version of SOS does not match the version of CLR you are debugging.  Please
load the matching version of SOS for the version of CLR you are debugging.
CLR Version: 4.0.30319.1
SOS Version: 4.0.30319.17929
CLRDLL: C:\Windows\Microsoft.NET\Framework64\v4.0.30319\mscordacwks.dll:4.0.30319.17929 f:8
doesn't match desired version 4.0.30319.01 f:8
```

## Just tell me what to do, already!

1. Copy `clr.dll`, `sos.dll` and `mscordacwks.dll` from the machine where the
   crash is occurring.
2. Put them in `C:\Temp`.
3. `.exepath+ C:\Temp`
4. `.load C:\Temp\sos`
5. Done, you can now `!vmstat`, etc., to your heart's content.
