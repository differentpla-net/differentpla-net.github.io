---
title: "Building a static library with Jam"
date: 2001-07-04T21:49:22.000Z
tags: jam
---

## Introduction

Once again, we're going to do what we did with the [MFC application]({% post_url 2001/2001-07-14-building-an-mfc-application-with-jam %}) and
[DLL]({% post_url 2001/2001-07-16-building-a-dll-with-jam %}) examples: build the project with AppWizard, and then get it built
with jam.

## Using AppWizard to generate the library

Run up Visual C++ and generate a new &quot;Win32 Static Library&quot; project.  Call it `win32_lib`,
and put it in the `S:\jam-test\lib\win32_lib` directory.  For now we want neither &quot;Pre-Compiled
header&quot; nor &quot;MFC support&quot;.

This gives us a project with no files in it.  We'll create a C++ file, and a header file
(`something.cpp` and `something.h`), and we'll create a
simple function:

```
/* something.cpp */

#include "something.h"
#include &lt;string.h&gt;

int something(const char *p)
{
    return strlen(p) + 42;
}
```

```
/* something.h */

int something(const char *p);
```

We'll add these to the Visual C++ project, and check that it builds, as we did with the other
examples: for sanity's sake.

## Creating the Jamfile

As we did with the other examples, we'll create a simple Jamfile.  We'll also add all of the SubDir stuff
necessary to integrate it into our overall build system:

```
SubDir TOP lib win32_lib ;

Library win32_lib : something.cpp ;
```

This builds, except that Jam says &quot;warning: lib depends on itself&quot;.  This is down to the fact that
one of jam's pseudotargets is called 'lib'.  The `Jamfile.html` documentation gives three different
options to resolve this conflict:

1. Change the name of the conflicting file.
2. Modify Jambase and change the name of the pseudotarget.
3. Use grist on the target name.

Now, I can't change the name of the conflicting file (or, in this case, directory) -- I'm looking to use jam
to build empeg's source code.  We've had a directory called lib for over two years.  It's in CVS with that name,
and there's over 180KLOC in there.  That rules out option one.

I haven't figured out the implications of using grist on the target name, and a brief look at the example suggests
that this isn't viable anyway, so that kinda leaves us with option 2 -- changing Jambase.  See
[here]({% post_url 2004/2004-01-09-conflicting-lib-target %}).

## Linking

Now we've built the static library, we'd better attempt to link with it.  This involves modifying the
[application]({% post_url 2001/2001-07-14-building-an-mfc-application-with-jam %}) we built earlier, so that it calls the
library.  We'll add this piece of code to `InitInstance`:

```
    int k = something("The quick brown fox jumps over the lazy dog");
```

We have to add a #include line, as well:

```
#include "win32_lib/something.h"
```

Of course, the compiler doesn't know how to resolve this relative path, so we'll fix it up in `Jamrules`:

```
HDRS += [ FDirName $(TOP) lib ] ;
```

We use the HDRS variable, rather than &quot;C++FLAGS += /I&quot; in order to tell Jam to search this directory
for header files when doing dependency scans.  Without this, it doesn't rebuild mfc_app.cpp when something.h
changes.

We use the `FDirName` function so that we don't have to worry about slash (Unix)
vs. backslash (DOS) vs. dot (VMS) vs. colon (Mac) -- not that we build on anything other than
NT or Unix at the moment (which both support slash).

## LinkLibraries

It still doesn't link, though: We've not told the linker about win32_lib.lib, so the application has
unresolved externals.  This is fixed with the LinkLibraries rule in `mfc_exe\Jamfile`:

```
Main mfc_exe : ChildFrm.cpp MainFrm.cpp mfc_exe.cpp mfc_exeDoc.cpp mfc_exeView.cpp StdAfx.cpp ;
Resource mfc_exe : mfc_exe.rc ;
LinkLibraries mfc_exe : win32_lib ;
```

This results in a warning from LINK about conflicting libraries.  We just lift the SUBDIRC++FLAGS
from `mfc_exe\Jamfile` and apply them to `win32_lib\Jamfile`, which fixes it.

## Linking Sideways

Currently, it's possible to build the project by invoking jam from the top-level directory.  We'd like
to be able to build a subset of the project by invoking jam from a subdirectory.  It's simple.  We just
add a `SubInclude` invocation to the relevant directory.  For example, `mfc_exe`
depends on `win32_lib`, so we just change `mfc_exe\Jamfile`:

```
SubDir TOP apps mfc_exe ;

SUBDIRC++FLAGS += /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" ;
RCFLAGS += /d "_DEBUG" /d "_AFXDLL" /l 0x809 ;

Main mfc_exe : ChildFrm.cpp MainFrm.cpp mfc_exe.cpp mfc_exeDoc.cpp mfc_exeView.cpp StdAfx.cpp ;
Resource mfc_exe : mfc_exe.rc ;
LinkLibraries mfc_exe : win32_lib ;

SubInclude TOP lib win32_lib ;
```

Note that we keep the `SubInclude` well clear of the rest of the Jamfile contents.
It should either occur at the end, or before the `SubDir` rule.

You can find the source resulting from this [here](/files/jam-test-20010713b.tar.gz).
