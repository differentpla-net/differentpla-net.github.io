---
title: "Many to One Dependencies"
date: 2001-07-10T09:44:50.000Z
tags: visual-studio
---

There are times when you want to have multiple input files in a Visual C++ project configured to generate the same
output file. For example, you want to catenate a bunch of files to generate a file to be #included.

The problem is that Visual C++ won't let you. You'll be prompted with the message:

> The source files "one.foo" and "two.foo" are both configured to produce the output file "out.foo". The project cannot
> be built.

This is irritating.

## The Problem

My problem: Take a smattering of files and catenate them before running them through the message compiler. When any of
the files has changed, rebuild the .mc file, and run the message compiler again.

The message compiler generates a header file (we plan on throwing this away), and a .rc file which should be included
into the application's .rc file.

## Using a Makefile Project

The major problem with this is getting the dependencies to work correctly. My initial idea was to use an "external
makefile" project, which runs nmake on a custom makefile. This seems to work. If any of the input files change, the
external project (which can use multiple source files in a rule) causes the output file to be built.

There's a problem, however. Since Visual C++ doesn't work out that one project is dependent on the output file generated
by the external makefile project, it doesn't build everything properly. You have to press F7 twice to get it to work.
The first time, the external project is run and regenerates the file, the second time, VC++ actually notices and
rebuilds the main project.

So this isn't ideal. Back to the drawing board.

## The solution : User-Defined Dependencies

After much poking around in Visual C++, I found the "Dependencies" button on the "Custom Build" tab.

It does exactly what it says on the tin. However, it's easy to get confused. What I did was this:

1. Add the .mc fragments to the project.
2. As a custom build step for each of the fragments, run a script that does the header-generation phase of the message
compiler. The output file from this is set to this generated header file. This is important for any .cpp files that
include it.
3. Create a file containing a list of the .mc fragments. This file could be empty. It's just there to hang some custom
build rules on. I'm using a list of the fragments for convenience - the catenation tool can handle response files.
4. Make this file explicitly dependent on the message fragment files.
5. The custom build step for this file catenates the files together to generate the .mc file. The output file is set to
the name of the .mc file. This ensures that Visual C++ knows how to generate the .mc file from the fragments. The
user-defined dependencies ensure that it rebuilds it when any of the fragments change.
6. Add the generated .mc file to the project, and add the custom build step to run the message compiler. The output
files should be set correctly. For example, if the generated .mc file is called `caten.mc`, the output files
are `caten.h`, `caten.rc` and `msg00001.bin`.
7. Go to View|Resource Includes and add `#include "caten.rc"` to the "Compile-time directives".

Eh voila!  It works. Changing any of the fragment files results in the header file and the .mc file being regenerated.
The message compiler is run correctly, and the resources are rebuilt properly.
