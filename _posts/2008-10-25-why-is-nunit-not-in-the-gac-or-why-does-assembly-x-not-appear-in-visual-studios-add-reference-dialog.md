---
title: "Why is NUnit not in the GAC? (or Why does [assembly X] not appear in Visual Studio's Add Reference dialog?)"
date: 2008-10-25T10:58:08.000Z
x-drupal-nid: 218
x-needs-review: 2008-10-25T10:58:08.000Z
---
Because Visual Studio doesn’t look for references in the GAC:

*   [http://blogs.msdn.com/msbuild/archive/2007/04/12/new-reference-assemblies-location.aspx](http://blogs.msdn.com/msbuild/archive/2007/04/12/new-reference-assemblies-location.aspx)
*   [http://blogs.msdn.com/junfeng/archive/2004/03/22/93708.aspx](http://blogs.msdn.com/junfeng/archive/2004/03/22/93708.aspx)

This is by design.

You can either add your files explicitly, which doesn’t work if other people in your team have installed the files somewhere else, for example `C:\Program Files\NUnit 2.4.6` vs. `C:\Program Files\NUnit-2.4.6`. Or `C:\Program Files\NUnit 2.4.6` vs. `C:\Program Files (x86)\NUnit 2.4.6`.

You have a couple of options:

*   If your assemblies don’t already have homes, you can put them in the VS `PublicAssemblies` folder: [http://blogs.msdn.com/csharpfaq/archive/2004/10/20/245239.aspx](http://blogs.msdn.com/csharpfaq/archive/2004/10/20/245239.aspx)
*   If they already have homes, you can add them to the `HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\.NETFramework\AssemblyFolders` registry key: [http://support.microsoft.com/?kbid=306149](http://support.microsoft.com/?kbid=306149).
*   If you’re developing assemblies for other developers to use (i.e. you’re Microsoft or a development tool vendor), you can put these in a subdirectory of `C:\Program Files\Reference Assemblies directory`, and then add that to the `AssemblyFolders` registry key.

This means that your project files will reference the assembly by name (i.e. name, version, public key token, all that jazz), and it won’t matter where it’s actually installed on your PC.

Note, however, that this doesn’t work as-is on 64-bit, because Visual Studio is a 32-bit application. You actually need to register your stuff under `HKEY_LOCAL_MACHINE\SOFTWARE\Wow6432Node\Microsoft\.NETFramework\AssemblyFolders`, and you should probably register under both.

There are also corresponding `HKEY_CURRENT_USER` variants of those keys, but (since that roams), it’s not much use (because the paths are usually relative to the machine, anyway).
