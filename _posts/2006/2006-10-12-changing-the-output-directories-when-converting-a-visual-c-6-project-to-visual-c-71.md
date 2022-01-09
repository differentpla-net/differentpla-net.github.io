---
title: "Changing the output directories when converting a Visual C++ 6 project to Visual C++ 7.1"
date: 2006-10-12T11:16:21.000Z
---
When you import a project from Visual C++ 6 into Visual Studio 2003 (i.e. Visual C++ 7.1), it doesn't fix up the output directories to use the correct definitions.

To fix it, you need to change the following entries, so that they have the correct paths in -- or you can specify `$(ConfigurationName)` or `$(IntDir)` or `$(OutDir)` instead.

*   General / Output Directory: This sets `$(OutDir)`. You should probably set it to `$(ConfigurationName)`
*   General / Intermediate Directory: This sets `$(IntDir)`. You should probably set it to `$(ConfigurationName)`
*   C/C++ / Precompiled Headers / Precompiled Header File: `$(IntDir)/$(TargetName).pch`
*   C/C++ / Output Files / ASM List Location: `$(IntDir)/`
*   C/C++ / Output Files / Object File Name: `$(IntDir)/`
*   C/C++ / Output Files / Program Database File Name: `$(IntDir)/vc70.pdb`
*   Linker / General / Output File: `$(OutDir)/$(ProjectName).exe`
*   Linker / Debugging / Generate Program Database File: `$(OutDir)/$(ProjectName).pdb`
*   Linker / Advanced / Import Library: `$(OutDir)/$(TargetName).lib`
*   Resources / Additional Include Directories: `$(IntDir)`
*   Resources / Resource File Name: `$(IntDir)/$(InputName).res`
*   MIDL / Output / Type Library: `$(IntDir)/$(ProjectName).tlb`

There might be other places you need to check, e.g. depending on whether you've got browser information turned on.
