---
layout: post
title: "PROCESSOR_ARCHITEW6432"
date: 2013-03-10T17:45:44.481Z
alias: /post/UTzGyH-mfVEoAAAB/processor_architew6432
---

What's the `PROCESSOR_ARCHITEW6432` environment variable for?

If you're on 64-bit Windows, you'll see that you have a `PROCESSOR_ARCHITECTURE` environment variable:

    C:\>echo %PROCESSOR_ARCHITECTURE%
	AMD64

This is from a 64-bit Command Prompt on 64-bit Windows.

If you run the 32-bit (x86) Command Prompt, you'll see something different:

	C:\>echo %PROCESSOR_ARCHITECTURE%
	x86

	C:\>echo %PROCESSOR_ARCHITEW6432%
	AMD64

Why is this important? Well, maybe you're running 32-bit MSBuild (because some of your tasks are in 32-bit DLLs), and you want to run 64-bit PowerShell (because you want to use a module that requires 64-bit).

From a 32-bit process on 64-bit Windows, you can't simply run `C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe`, because `System32` is mapped to `SysWOW64`, meaning that you'll get the 32-bit PowerShell.

No, to get a 64-bit PowerShell from a 32-bit process, you need to run `C:\Windows\SysNative\WindowsPowerShell\v1.0\powershell.exe`, because `SysNative` is mapped to the correct `System32` directory.

However, you can't use `SysNative` from a 32-bit process on 32-bit Windows, because it doesn't exist.

Hence the need to distinguish. If you're in a .NET 4.0 process (or PowerShell 3.0), you can use the `Environment.Is64BitOperatingSystem` property.

From MSBuild (or Command Prompt), you need to use the presence of the `PROCESSOR_ARCHITEW6432` environment variable. If it's set, then you're in a 32-bit process on 64-bit Windows, and must use `SysNative`. If it's not set, then you're either in a 32-bit process on 32-bit Windows, or in a 64-bit process on 64-bit Windows. In that case, you can use `System32`.
