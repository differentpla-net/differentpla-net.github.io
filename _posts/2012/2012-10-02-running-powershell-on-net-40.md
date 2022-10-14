---
title: "Running PowerShell on .NET 4.0"
date: 2012-10-02T12:39:18.000Z
tags: powershell
---
Attempting to use **Add-Type** to load a .NET assembly in PowerShell this afternoon, I got the following error:

<pre>This assembly is built by a runtime newer than the currently loaded runtime</pre>

By default, PowerShell runs using the v2.0 .NET runtime, and my assembly is built against .NET 4.0\. To get this to work, we need to persuade PowerShell to run on .NET 4.0

The best way to do this is the [second answer](http://stackoverflow.com/a/5069146/8446) on this Stack Overflow question. In short, you add a **PowerShell.exe.config** file that loads .NET 4.0.

You can check that this has worked by looking at the **$PSVersionTable.CLRVersion** variable in PowerShell.
