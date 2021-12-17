---
title: "Building a DLL with Jam"
date: 2001-07-16T17:29:57.000Z
tags: jam
---
## Introduction

empeg's code base contains several Windows DLL projects. I'm going to investigate getting these built with jam the [same way]({% post_url 2001-07-14-building-an-mfc-application-with-jam %}) as I did for the MFC applications -- by building a simple project with the AppWizard, and getting the project built.

## Using AppWizard to generate the DLL

As I've already stated, our DLLs don't use MFC, so we'll use the AppWizard to generate a "Win32 Dynamic-Link Library". To mirror the structure used at empeg, we'll create the DLL in the `S:\jam-test\lib\` directory, and we'll call it `win32_dll`.

The AppWizard wants to know what type of DLL we're creating, so we'll create one that exports some symbols. We'll build it with Developer Studio, just to check.

## Creating the Jamfile

The obvious thing to start with is a Jamfile that looks like this:

```
Main win32_dll : StdAfx.cpp win32_dll.cpp ;
```

The `Main` rule section in the `Jambase` file is geared up to create a .EXE file, so we'll have to hack on it a little. If we just copy it to a new rule called `SharedLibrary`, we can make our changes there. It turns out that the only change we need to make is the suffix of the generated file:

```
	_t = [ FAppendSuffix $(<) : $(SUFSHR) ] ;
```

We have to go and define SUFSHR somewhere, though. The definition of the `SharedLibrary` rule is [here]({% post_url 2001-07-04-building-shared-libraries-dlls %}).
Once we've added the new rules to Jambase, and changed "Main" to "SharedLibrary" in our Jamfile, we get the following:

```
win32_dll.cpp(25) : error C2491: 'nWin32_dll' : definition of dllimport data not allowed
win32_dll.cpp(29) : error C2491: 'fnWin32_dll' : definition of dllimport function not allowed
win32_dll.cpp(36) : warning C4273: 'CWin32_dll::CWin32_dll' : inconsistent dll linkage.  dllexport assumed.
```

This doesn't work, so we'll take a look at the compiler command lines.

## Compiler Flags

Jam is invoking the compiler like this:

```
cl /nologo /c /Fowin32_dll.obj /IP:\VStudio\VC98\include /Tpwin32_dll.cpp
```

Developer Studio is invoking the compiler like this:

```
cl /nologo /MTd /W3 /Gm /GX /ZI /Od
	/D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "WIN32_DLL_EXPORTS"
	/Fp"Debug/win32_dll.pch" /Yu"stdafx.h" /Fo"Debug/" /Fd"Debug/"
	/FD /GZ /c
```

We looked at what the compiler flags meant [here]({% post_url 2001-07-14-building-an-mfc-application-with-jam %}). The only one that's different is `/MTd`, rather than `/MDd`. This is building a multi-threaded application, and linking it to LIBCMT.LIB rather than to MSVCRT.LIB. Also promising are the `/D` switches. We'll put those into our Jamfile:

```
C++FLAGS += /MTd /W3 /Gm /GX /ZI /Od
			/D "WIN32" /D "_DEBUG" /D "_WINDOWS"
			/D "_MBCS" /D "_USRDLL" /D "WIN32_DLL_EXPORTS" ;

SharedLibrary win32_dll : StdAfx.cpp win32_dll.cpp ;
```

This compiles happily, but the linker leaves us with:

```
LINK : fatal error LNK1561: entry point must be defined
```

## Entry Point

Jam is invoking link like this:

```
link /nologo /out:win32_dll.dll StdAfx.obj win32_dll.obj
```

Developer Studio is invoking link with a response file containing the following:

```
kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib
ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib
/nologo /dll /incremental:yes /pdb:"Debug/win32_dll.pdb" /debug /machine:I386
/out:"Debug/win32_dll.dll" /implib:"Debug/win32_dll.lib" /pdbtype:sept
.\Debug\StdAfx.obj
.\Debug\win32_dll.obj
```

We'll add some of the more interesting switches to our Jamfile, and see what happens:

```
C++FLAGS += /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "WIN32_DLL_EXPORTS" ;
LINKFLAGS += /dll /incremental:yes /debug /machine:I386 ;

SharedLibrary win32_dll : StdAfx.cpp win32_dll.cpp ;
```

It builds!
It seems to me that the extra LINKFLAGS should really be added in the SharedLibrary rule, rather than here. Also, for some reason, Visual C++ is passing a `/implib` switch to the linker which we didn't seem to need (it built an import library correctly). I'll come back to this later.

You can find the resulting source code from this example [here](../src/jam-test-20010712a.tar.gz).
