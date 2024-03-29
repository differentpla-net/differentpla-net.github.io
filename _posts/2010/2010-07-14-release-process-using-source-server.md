---
title: "Release Process: Using Source Server"
date: 2010-07-14T13:33:54.000Z
---
One of the cool things you can do with the Microsoft Debugging Tools is set up a source server. This works by embedding source control details in your PDB files. Once you've published these to a symbol server, and set that up properly, you should be able to load a minidump file in Visual Studio (or WinDbg) and be taken to the correct line of the correct revision of the source file where the crash happened.

Which is pretty cool.

The scripts are in the Microsoft [Debugging Tools for Windows](http://www.microsoft.com/whdc/devtools/debugging/default.mspx). By default, you'll find them in `C:\Program Files (x86)\Microsoft Debugging Tools for Windows (x86)\srcsrv`.

They need Perl, and they [don't work]({% post_url 2009/2009-04-23-source-server-indexing-fails-with-svn-cant-get-details-for-sourcefile %}) with a default installation of [Cygwin](http://www.cygwin.com/), so you'll need to install [ActiveState Perl](http://www.activestate.com/activeperl/) instead. Make sure it appears in your PATH variable ahead of the Cygwin one.

Then, to index your source, change to your source directory, and run SSINDEX.CMD, passing (at least) the -System switch. If you're using Subversion, it should work as-is. If you're using Perforce or (heaven forbid) Visual SourceSafe, you'll need to enter some server details in SRCSRV.INI. Use the -Debug switch to see what's happening.
