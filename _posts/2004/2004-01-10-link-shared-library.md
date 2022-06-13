---
title: "Linking with a shared library"
date: 2001-07-04T21:49:22.000Z
tags: jam
---

## Introduction

Now we've got our [application]({% post_url 2001/2001-07-14-building-an-mfc-application-with-jam %}) and [DLL]({% post_url 2001/2001-07-16-building-a-dll-with-jam %}) building as part of the same project, we really ought to persuade them to link together.

The first thing we ought to do is establish some kind of dependency in the code.  It's test-first, but for Jamfiles.
We'll do this by attempting to use one of the symbols exported from the DLL in our MFC application.  We'll add this
piece of code to `InitInstance`:

```
    int j = fnWin32_dll();
```

This requires that we include the relevant header file:

```
#include "win32_dll/win32_dll.h"
```

It now compiles correctly, but fails to link.  We need to establish a dependency on the import library generated as part
of the DLL build process.  We can add this to the `mfc_exe\Jamfile`:

```
LinkLibraries mfc_exe : win32_lib win32_dll ;
```

This fails because jam doesn't know how to build win32_dll.lib.  We need to add some extra stuff to
the `SharedLibaryFromObjects` rule:

```
        MakeLocate $(_t) : $(LOCATE_TARGET) ;

        <b># Tell jam where it can find the import library
        MakeLocate $(_t:S=$(SUFLIB)) : $(LOCATE_TARGET) ;</b>

        Clean clean : $(_t) ;
```

This compiles and links.  It even runs -- if we make sure that the DLL can be found when
loading the EXE.

If we use shared libraries on Unix, we'll have to invent a `SharedLinkLibraries` rule,
because `gcc` wants the name of the `.so` file in order to establish the
runtime dependency, and `LinkLibraries` assumes `.lib`.

Source code is [here](/files/jam-test-20010716a.tar.gz).
