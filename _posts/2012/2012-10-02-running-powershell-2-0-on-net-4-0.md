---
title: Running PowerShell 2.0 on .NET 4.0
date: 2012-10-02T12:39:00Z
---
Attempting to use `Add-Type` to load a .NET assembly in PowerShell this afternoon, I got the following error:

    This assembly is built by a runtime newer than the currently loaded runtime
    
By default, PowerShell 2.0 runs using the v2.0 .NET runtime, and my assembly is built
against .NET 4.0. To get this to work, we need to persuade PowerShell to run on .NET 4.0

Note that PowerShell 3.0 uses the .NET 4.0 runtime, so you don't need to do anything.

The best way to do this is the [second answer](http://stackoverflow.com/a/5069146/8446)
on this Stack Overflow question. In short, you add a `PowerShell.exe.config` file that loads .NET 4.0.

You can check that this has worked by looking at the `$PSVersionTable.CLRVersion` variable in PowerShell.
