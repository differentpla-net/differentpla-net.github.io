---
title: "Jam SubDir Rule"
date: 2001-07-13T18:25:38.000Z
tags: jam
---

## Introduction

Now we've built [an MFC application]({% post_url 2001/2001-07-14-building-an-mfc-application-with-jam %})
and [a DLL]({% post_url 2001/2001-07-16-building-a-dll-with-jam %}), we'd like to include them in
the same build process. This is what Jam's `SubDir` rule does.

In the top-level directory, i.e. `jam-test`, we place a `Jamfile` looking like this:

```
SubDir TOP ;

SubInclude TOP lib ;
SubInclude TOP apps ;
```

We also need to create an empty `Jamrules` file, in order to supress a warning. This is not a problem: we'll probably be
putting project-specific rules in there in a moment.


We also have to create corresponding `Jamfile` files for the other directories. The one in the `apps` directory looks
like this:

```
SubDir TOP apps ;

SubInclude TOP apps mfc_exe ;
```

And we have to add `SubDir` invocations to the top of the original Jamfiles, so that they know where they are.

We run the build from the top-level directory:

```
S:\jam-test>jam -d2 -f /jam-test/Jambase
Compiler is Microsoft Visual C++
...found 116 target(s)...
...updating 8 target(s)...
C++ apps\mfc_exe\ChildFrm.obj

        cl /nologo  /c  /MTd /W3 /Gm /GX /ZI /Od /D WIN32 /D _DEBUG /D _WINDOWS /D _MBCS /D _USRDLL /D WIN32_D
LL_EXPORTS /MDd /W3 /Gm /GX /ZI /Od /D WIN32 /D _DEBUG /D _WINDOWS /D _AFXDLL /D _MBCS    /Foapps\mfc_exe\Chil
dFrm.obj  /Iapps\mfc_exe  /IP:\VStudio\VC98\include  /Tpapps\mfc_exe\ChildFrm.cpp

Command line warning D4025 : overriding '/MTd' with '/MDd'
ChildFrm.cpp

... etc. ...

Rc apps\mfc_exe\mfc_exe.res

        rc  /d _DEBUG /d _AFXDLL /l 0x809    /Fo apps\mfc_exe\mfc_exe.res  apps\mfc_exe\mfc_exe.rc

Link apps\mfc_exe\mfc_exe.exe

        link /nologo   /dll /incremental:yes /debug /machine:I386 /subsystem:windows /incremental:yes /debug /
machine:I386  /out:apps\mfc_exe\mfc_exe.exe   apps\mfc_exe\ChildFrm.obj apps\mfc_exe\MainFrm.obj apps\mfc_exe\
mfc_exe.obj apps\mfc_exe\mfc_exeDoc.obj apps\mfc_exe\mfc_exeView.obj apps\mfc_exe\StdAfx.obj  apps\mfc_exe\mfc
_exe.res

...updated 8 target(s)...
```

If we attempt to run the `mfc_exe` executable, we find that it's "not a valid Win32 executable". This would
appear to be down to the `/dll` switch being passed to the linker: we've managed to build a DLL and give it a .exe
extension.

This behaviour is down to the way that Jam runs.  Unlike recursive make, jam loads all of the named and included
Jamfiles into the same "namespace".  Thus, `C++FLAGS` and `LINKFLAGS` are persistant from one Jamfile to the next.

At this point, we do something we should have done before:  Since we're invoking our `SharedLibrary` rule to build the
DLL, we should add the `/dll` switch to the linker command line at that point.

We also don't like the "overriding '/MTd' with '/MDd'" warning.  At this point, we introduce the
`SUBDIRC++FLAGS` rule.  It's like `C++FLAGS`, but the flags only stay in effect until the next SubDir rule.  We should
change the DLL and executable Jamfiles to use this.  For example, `mfc_exe\Jamfile` looks like this:

```
SubDir TOP apps mfc_exe ;

SUBDIRC++FLAGS += /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" ;
LINKFLAGS += /subsystem:windows /incremental:yes /debug /machine:I386 ;
RCFLAGS += /d "_DEBUG" /d "_AFXDLL" /l 0x809 ;

Main mfc_exe : ChildFrm.cpp MainFrm.cpp mfc_exe.cpp mfc_exeDoc.cpp mfc_exeView.cpp StdAfx.cpp ;
Resource mfc_exe : mfc_exe.rc ;
```

We also note that the LINKFLAGS are accumulating.  There's (strangely) no SUBDIRLINKFLAGS corresponding to
SUBDIRC++FLAGS, but it's not a major problem.  The flags only differ slightly, and that's according to the exe/dll
nature of the target.  We can move them into `Jambase`:

```
rule MainFromObjects
{
	local _s _t ;

	# Add grist to file names
	# Add suffix to exe

	_s = [ FGristFiles $(>) ] ;
	_t = [ FAppendSuffix $(<) : $(SUFEXE) ] ;

	if $(_t) != $(<)
	{
	    DEPENDS $(<) : $(_t) ;
	    NOTFILE $(<) ;
	}

	# make compiled sources a dependency of target

	DEPENDS exe : $(_t) ;
	DEPENDS $(_t) : $(_s) ;
	MakeLocate $(_t) : $(LOCATE_TARGET) ;

	Clean clean : $(_t) ;

	<b>LINKFLAGS on $(_t) += /subsystem:windows /incremental:yes /debug /machine:I386 ;</b>
	Link $(_t) : $(_s) ;
}
```

```
rule SharedLibraryFromObjects
{
	local _s _t ;

	# Add grist to file names
	# Add suffix to dll

	_s = [ FGristFiles $(>) ] ;
	_t = [ FAppendSuffix $(<) : $(SUFSHR) ] ;

	if $(_t) != $(<)
	{
	    DEPENDS $(<) : $(_t) ;
	    NOTFILE $(<) ;
	}

	# make compiled sources a dependency of target

	DEPENDS exe : $(_t) ;
	DEPENDS $(_t) : $(_s) ;
	MakeLocate $(_t) : $(LOCATE_TARGET) ;

	Clean clean : $(_t) ;

	<b>LINKFLAGS on $(_t) += /dll /incremental:yes /debug /machine:I386 ;</b>
	Link $(_t) : $(_s) ;
}
```

## Conclusions

That was easy enough.  We've still got a couple of things to work out:

- We're not actually attempting to link the EXE with the DLL, so there's no dependency established.
- The resource compiler flags aren't being reset.  We've only got one resource script so far, so it's not an issue yet.

You can find the source resulting from this [here](/files/jam-test-20010713a.tar.gz).
