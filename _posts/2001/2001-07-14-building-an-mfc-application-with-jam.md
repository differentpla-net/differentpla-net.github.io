---
title: "Building an MFC Application with Jam"
date: 2001-07-14T00:25:38.000Z
tags: jam
---
## Introduction

Since the majority of our Windows applications are written using MFC, it's a useful experiment to get jam to build a freshly-generated MFC application. Once we've got this working, we can turn our attention to the things that make our build process different.

I've attempted to break down the process of getting an MFC application to build into discrete chunks. They're not particularly self-contained at the moment, but they attempt to describe one problem (and hopefully its solution) each:

You can find the resulting source code from this example [here](/files/jam-test-20010711a.tar.gz).

## Using AppWizard to generate the application

Run up Visual C++, and generate a new "MFC AppWizard (exe)" project. I'm going to be replicating parts of our build system around it, so I called it `mfc_exe` and put it in `s:\jam-test\apps\mfc_exe`. The default settings for the application are fine, so just keep clicking "Next".

When you've got your application generated, get Visual C++ to build it, just for sanity's sake.

## Creating the Jamfile

The obvious thing to do at this point is to put the names of the .cpp files into a Jamfile, like this:

```
Main mfc_exe : ChildFrm.cpp MainFrm.cpp mfc_exe.cpp mfc_exeDoc.cpp mfc_exeView.cpp StdAfx.cpp ;
```

...and then to try building it, using -d2 to see what's going on. Not surprisingly, it doesn't work:

```
Link mfc_exe.exe
nafxcw.lib(afxmem.obj) : error LNK2005: "void * __cdecl operator new(unsigned int)" (??2@YAPAXI@Z) already def
ined in libc.lib(new.obj)
nafxcw.lib(afxmem.obj) : error LNK2005: "void __cdecl operator delete(void *)" (??3@YAXPAX@Z) already defined
in libc.lib(delete.obj)
libc.lib(crt0.obj) : error LNK2001: unresolved external symbol _main
nafxcw.lib(thrdcore.obj) : error LNK2001: unresolved external symbol __endthreadex
nafxcw.lib(thrdcore.obj) : error LNK2001: unresolved external symbol __beginthreadex
mfc_exe.exe : fatal error LNK1120: 3 unresolved externals

link /nologo    /out:mfc_exe.exe   ChildFrm.obj MainFrm.obj mfc_exe.obj mfc_exeDoc.obj mfc_exeView.obj StdAfx.
obj   P:\VStudio\VC98\lib\advapi32.lib P:\VStudio\VC98\lib\libc.lib P:\VStudio\VC98\lib\oldnames.lib P:\VStudi
o\VC98\lib\kernel32.lib

...failed Link mfc_exe.exe ...
```

## Compiler Flags

Essentially, jam's invocation of Visual C++ isn't using the multithreaded libraries (the __beginthreadex unresolved external), and there's something else wrong with the implicit link instructions.

Let's take a look at the compiler settings and see what's different. Jam is invoking the compiler like this:

```
cl /nologo /c /FoChildFrm.obj /IP:\VStudio\VC98\include /TpChildFrm.cpp
```

Developer Studio is invoking the compiler like this (taken from the .plg file):

```
cl /nologo /MDd /W3 /Gm /GX /ZI /Od
    /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS"
    /Fp"Debug/mfc_exe.pch" /Yu"stdafx.h" /Fo"Debug/" /Fd"Debug/"
    /FD /GZ   /c ChildFrm.cpp
```

Looking at the results of `cl /?` tells us the following:

<table>
<thead>
 <tr>
  <th>Switch</th>
  <th>Description</th>

  <th>Switch</th>
  <th>Description</th>
 </tr>
</thead>
<tbody>
 <tr>
  <td>/nologo</td>
  <td>Don't output a version banner</td>

  <td>/MDd</td>
  <td>Link with the MSVCRTD.lib file.</td>
 </tr>
 <tr>
   <td>/W3</td>
   <td>Set the warning level</td>

   <td>/Gm</td>
   <td>Enable minimal rebuild</td>
 </tr>
 <tr>
  <td>/GX</td>
  <td>Enable exceptions</td>

  <td>/ZI</td>
  <td>Enable Edit and Continue debug info</td>
 </tr>
 <tr>
  <td>/Od</td>
  <td>Disable optimisations (debug)</td>

  <td>/D</td>
  <td>Define some stuff</td>
 </tr>

 <tr>
  <td>/Fp</td>
  <td>Name precompiled header file</td>

  <td>/Yu</td>
  <td>Use .PCH file</td>
 </tr>

 <tr>
  <td>/Fo</td>
  <td>Name object file</td>

  <td>/Fd</td>
  <td>Name .PDB file</td>
 </tr>

 <tr>
  <td>/FD</td>
  <td>Generate file dependencies</td>

  <td>/GZ</td>
  <td>Enable runtime debug checks</td>
 </tr>

 <tr>
  <td>/c</td>
  <td>Don't link; just compile</td>

  <td>/I</td>
  <td>Name include directory</td>
 </tr>

 <tr>
  <td>/Tp</td>
  <td>Treat the file as C++</td>
 </tr>
</tbody>
</table>

Obviously, we'd like the warnings and debug information. We'll probably need the `/D` switches, as well. Diagnosing the
error messages above suggests that we'll need `/MDd`. For now we can ignore the precompiled header stuff, and we'll come
back to the file naming things.

That leaves us with a file looking like this:

```
C++FLAGS += /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" ;

Main mfc_exe : ChildFrm.cpp MainFrm.cpp mfc_exe.cpp mfc_exeDoc.cpp mfc_exeView.cpp StdAfx.cpp ;
```

...and the following results:

```
LINK : warning LNK4098: defaultlib "msvcrtd.lib" conflicts with use of other libs; use /NODEFAULTLIB:library
libc.lib(crt0.obj) : error LNK2001: unresolved external symbol _main
mfc_exe.exe : fatal error LNK1120: 1 unresolved externals
```

...which looks like it's caused by `Jambase` adding libraries we don't want.

## Link Libraries

We'll copy `Jambase` from the distribution directory and put it in into `S:\jam-test`, which is where it'll end up in our final build system. Looking through the file reveals the following:

```
    else if $(MSVCNT)
    {
    ECHO "Compiler is Microsoft Visual C++" ;

    AR      ?= lib ;
    AS      ?= masm386 ;
    CC      ?= cl /nologo ;
    CCFLAGS     ?= "" ;
    C++     ?= $(CC) ;
    C++FLAGS    ?= $(CCFLAGS) ;
    LINK        ?= link /nologo ;
    LINKFLAGS   ?= "" ;
    LINKLIBS    ?= $(MSVCNT)\\lib\\advapi32.lib
                $(MSVCNT)\\lib\\libc.lib
                $(MSVCNT)\\lib\\oldnames.lib
                $(MSVCNT)\\lib\\kernel32.lib ;
    OPTIM       ?= "" ;
    STDHDRS     ?= $(MSVCNT)\\include ;
    UNDEFFLAG   ?= "/u _" ;
    }
```

We'll take out the LINKLIBS line, leaving it looking like this:

```
    LINKLIBS    ?= "" ;
```

Remembering to invoke jam as: `jam -f /jam-test/Jambase` leaves us with this:

```
LINK : fatal error LNK1561: entry point must be defined
```

## Entry Point

Jam is invoking link like this:

```
link /nologo /out:mfc_exe.exe ChildFrm.obj MainFrm.obj mfc_exe.obj mfc_exeDoc.obj mfc_exeView.obj
 StdAfx.obj
```

Developer Studio is invoking link with a response file containing the following:

```
/nologo /subsystem:windows /incremental:yes /pdb:"Debug/mfc_exe.pdb"
/debug /machine:I386 /out:"Debug/mfc_exe.exe" /pdbtype:sept
.\Debug\mfc_exe.obj
.\Debug\StdAfx.obj
.\Debug\MainFrm.obj
.\Debug\ChildFrm.obj
.\Debug\mfc_exeDoc.obj
.\Debug\mfc_exeView.obj
.\Debug\mfc_exe.res
```

We'll add some of the more interesting switches to our Jamfile, and see what happens:

```
C++FLAGS += /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" ;
LINKFLAGS += /subsystem:windows /incremental:yes /debug /machine:I386 ;

Main mfc_exe : ChildFrm.cpp MainFrm.cpp mfc_exe.cpp mfc_exeDoc.cpp mfc_exeView.cpp StdAfx.cpp ;
```

It builds! Does it run? It does. Unfortunately, it bails out immediately. It should have brought up a window of some kind. Perhaps if we run it in the debugger?

## Resource Files

Running our newly-built MFC application in the debugger reveals the following smoking gun in the output window:

```
Warning: no document names in string for template #129.
Warning: no document names in string for template #129.
Warning: no shared menu for document template #129.
Warning: no document names in string for template #129.
Warning: no shared menu for document template #129.
Warning: failed to load menu for CFrameWnd.
```

Looks to me like it's not linking in the resource files. We'd better sort that out now. What we'd like to do is simply add the `.rc` file to the list of source files in the Jamfile, and have it magically work. However, when we try that, we get:

```
Unknown suffix on mfc_exe.rc - see UserObject rule in Jamfile(5)
```

Looking in the `Jambase.html` file included in the distribution, we find a section that suggests overriding the `UserObject` rule in order to tell jam about .rc files. It says to put it in `Jamrules`, but since we don't have one, we'll put it in our `Jamfile` for the time being:

```
RC ?= rc ;

C++FLAGS += /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" ;
LINKFLAGS += /subsystem:windows /incremental:yes /debug /machine:I386 ;
RCFLAGS += /d "_DEBUG" /d "_AFXDLL" ;

rule UserObject
{
    switch $(>)
    {
    case *.rc   : ResourceCompiler $(<) : $(>) ;
    case *      : EXIT "Unknown suffix on" $(>) "- see UserObject rule in Jamfile(5)." ;
    }
}

rule ResourceCompiler
{
    DEPENDS $(<) : $(>) ;
    Clean clean : $(<) ;
}

actions ResourceCompiler
{
    $(RC) /l 0x809 /fo $(<) $(RCFLAGS) $(>)
}

Main mfc_exe : ChildFrm.cpp MainFrm.cpp mfc_exe.cpp mfc_exeDoc.cpp mfc_exeView.cpp StdAfx.cpp mfc_exe.rc ;
```

Unfortunately, this approach is flawed: The generated file is called .obj, rather than .res. This causes a major problem in AppWizard-generated MFC applications, because the .rc file has the same base name as the main application source file, and they're both configured to generate a file with the .obj suffix.

Thus, one of the build steps will overwrite the output from the other. This is a bad thing.

It looks like we'll have to give up on our ideal of simply adding the filename to the list of .cpp files -- at least until we can figure out the magic in the `Main` rule in `Jambase`. Since the odds of that happening are slim, we'll cast our net a little further afield.

## Resource Files: A working solution

One of the mailing list participants, Chris Antos, forwarded me a copy of his Jambase file a little while ago. It contains all sorts of useful rules, but I'm not entirely sure what some of it does yet.

Lifting the relevant sections (and simplifying them) results in the following:

```
# Resource prog : resources.rc ;
rule Resource
{
    # _s is the source (.rc) file.
    # _r is the target (.res) file.
    # _e is the executable (.exe) file.

    local _s = [ FGristFiles $(>) ] ;
    local _r = $(_s:S=.res) ;       # Chris Antos has some stuff here to set grist ...:G=)
    local _e = [ FAppendSuffix $(<) : $(SUFEXE) ] ;

    # Make the executable depend on the .res file, and
    # make the .res file depend on the .rc file.
    DEPENDS $(_e) : $(_r) ;
    DEPENDS $(_r) : $(_s) ;

    LOCATE on $(_r) = $(LOCATE_TARGET) ;
    SEARCH on $(_s) = $(SEARCH_SOURCE) ;
    NEEDLIBS on $(_e) += $(_r) ;

    # TODO: Header file scanning.

    Rc $(_r) : $(_s) ;

    Clean clean : $(<) ;
}

actions Rc
{
    $(RC) $(RCFLAGS) /I$(HDRS) /I$(RCHDRS) /Fo $(<) $(>)
}
```

This works fine. If we build the program, we get a working executable!

## What Next?

We've just successfully built an MFC application using jam. There are a couple of things that we still need to consider:

* Resource Scripts can have include files. We've not done anything about dependency checking on them.
* Separate Debug/Release configurations.
* Visual C++ puts the output in Debug or Release. Currently, we just dump everything in the current directory.
* The AppWizard-generated project used precompiled headers. We ought to, as well. They dramatically improve compilation speed.

I'll come back to this later and deal with some of the above points.
