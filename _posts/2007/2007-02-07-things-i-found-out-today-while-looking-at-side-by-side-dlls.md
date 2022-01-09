---
title: "Things I found out today while looking at Side-by-Side DLLs"
date: 2007-02-07T16:37:45.000Z
---
*   You can add DLL dependencies to your EXE file's manifest by using the `/manifestdependency` linker switch. As with several other linker switches, this can be done using `#pragma comment(linker, ...)`.
*   The Visual C++ 2005 compiler automatically inserts a `/manifestdependency` switch into .OBJ files that it creates. It marks them as dependent on the Microsoft.VC80.CRT DLL, version 8.0.50727.762\. It does this in the VC\include\crtdefs.h file.
*   You can tell it to use the RTM version (8.0.50608.0) of the runtime by defining the _USE_RTM_VERSION macro. See VC\include\crtassem.h. The behaviour of this macro can be confirmed by opening (e.g.) stdafx.obj in a hex editor.
*   This also adds the /include:__forceCRTManifestRTM switch to the linker. I've not yet found out what this does.
*   MFC and ATL do something similar in the VC\atlmfc\include\atlassem.h and MFCassem.h files.
*   In order to allow you to merge your own manifest files, the linker is asked to create the _project_.exe.intermediate.manifest file, using the /MANIFESTFILE switch.
*   This is then fed to the MT.EXE utility, which can be controlled in the project property pages, under "Manifest Tool". The .intermediate.manifest file is added to the list implicitly.
*   If you add your own .manifest files to the project, these are also added to the MT.EXE command line implicitly.
*   MT.EXE is almost as bad as MIDL when it comes to error messages. If you include an empty or malformed manifest file in your project, it simply reports "general error c101008f: Failed to compile manifest(s). The parameter is incorrect.". It doesn't tell you which file or line caused the problem.
*   As an aside, the "No files were found to look in" misfeature still exists in Visual Studio 2005\. Press [Ctrl+Scroll Lock](http://blogs.ugidotnet.org/franny/archive/2005/12/08/31303.aspx) to fix it.
*   I don't actually have a copy of the RTM VC80.CRT DLLs on my Windows XP box, anyway. I have versions 8.0.50727.42 and 8.0.50727.762\. One of these would appear to correspond to the RTM version of Visual Studio, and the other to SP1\. I'm not sure about this, though.
*   These live in the `C:\Windows\WinSxS` folder. They both have extremely long names. For example, the latest one lives in a folder called `x86_Microsoft.VC80.CRT_1fc8b3b9a1e18e3b_8.0.50727.762_x-ww_6b128700`.
*   `In the C:\Windows\WinSxS\Policies\x86_policy.8.0.Microsoft.VC80.CRT_1fc8b3b9a1e18e3b_x-ww_77c24773` directory, there's a couple of policy files. The first one says that any CRT versions between 8.0.41204.256 and 8.0.50608.0 should be redirected to version 8.0.50727.42\. Presumably this covers a range containing all of the CTPs and betas up to the RTM version.
*   The second policy file appears to override this by saying that these same versions, and everything from there to 8.0.50727.762 should use the 8.0.50727.762 version.
*   I found the blog of the [man responsible](http://blogs.msdn.com/martynl/archive/2005/10/13/480880.aspx). He's not updated it since 2005 :-(
*   There's a tool called FUSLOGVW.EXE which is supposed to help diagnose problems with Side-by-Side DLLs.
