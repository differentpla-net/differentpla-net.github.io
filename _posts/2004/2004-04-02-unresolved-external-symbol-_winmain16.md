---
title: "Unresolved external symbol _WinMain@16"
date: 2004-04-02T08:55:00.000Z
---
I got this error message today:

<pre>msvcrt.lib(crtexew.obj) : error LNK2019:
    unresolved external symbol _WinMain@16 referenced in function _WinMainCRTStartup</pre>

The most common cause for this error message is adding Unicode configurations to your project and forgetting to change the entry point to `wWinMainCRTStartup`. See [this Microsoft KB article](http://support.microsoft.com/default.aspx?scid=kb;EN-US;125750) (or [my article](/node/view/135)) for more information.

This wasn't my problem: I'd already done that.

The puzzling thing was that the error was intermittent: I'd get it when building the project as part of a batch build; then I wouldn't; then it'd keep happening if I built the project on its own. I'd clean the project, and then it'd work. Then if I did a batch build, the problem would come back again.

It turns out that when you create a new configuration, and base it on an existing one -- I created the "Debug Unicode" configuration, and based it on the "Debug" configuration -- Visual Studio doesn't always correctly update all of the paths in the project settings. This means that the "Debug" and "Debug Unicode" configurations were sharing precompiled headers, debug information and other stuff. No wonder the linker was getting confused.

Admittedly, Visual Studio.NET's confusion could be due to me importing the project from a Visual Studio 6 project.

To fix it, you need to change the following entries, so that they have the correct paths in -- or you can specify `$(ConfigurationName)` or `$(IntDir)` or `$(OutDir)` instead.

*   General / Output Directory: This sets `$(OutDir)`. You should probably set it to `$(ConfigurationName)`.
*   General / Intermediate Directory: This sets `$(IntDir)`. You should probably set it to `$(ConfigurationName)`.
*   C/C++ / Precompiled Headers / Precompiled Header File: You should set this to `$(IntDir)/$(TargetName).pch`.
*   C/C++ / Output Files / Object File Name: Set this to `$(IntDir)/`.
*   C/C++ / Output Files / Program Database File Name: Set this to `$(IntDir)/vc70.pdb`.
*   Linker / General / Output File: `$(OutDir)/$(ProjectName).exe`.
*   Linker / Debugging / Generate Program Database File: `$(OutDir)/$(ProjectName).pdb`.
*   Resources / Additional Include Directories: `$(IntDir)`.
*   Resources / Resource File Name: `$(IntDir)/$(InputName).res`.

There might be other places you need to check, e.g. depending on whether you've got browser information turned on.
