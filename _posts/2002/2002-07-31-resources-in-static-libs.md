---
title: "Including resources in Static Library projects"
date: 2002-07-31T17:11:03Z
tags: mfc
redirect_from: /~roger/devel/tips/vc/resources_in_libs/
---

## Introduction

Consider the following situation:

You've got an MFC application, which uses a lot of static libraries to do a lot of the work. These libraries need to
interact with the user (perhaps they've got dialog boxes in them). Unfortunately, Visual C++ and MFC effectively force
you to store all of your resources in the main application .rc file. This makes it difficult to use the static library
(with the same resources) in multiple projects. Moreover, it makes the library dependent on the main application, for
the resource IDs.

The solution is relatively simple, if you're paying attention.

## Creating a Resource Script for the Static Library project

Go to File / New, and create a new "Resource Script" file. Add it to the static library project. Give it a vaguely
sensible name.

![](/images/2002/2002-07-31-resources-in-static-libs/new_resource_script.png)

By default, Visual Studio names the resource header "resource.h". This will cause some confusion with the resource.h
included by the main application, so rename it to something based on the library name. In this example, I've renamed it
to "LibA_rc.h".

Having renamed the header file, you'll also need to fix up some other things:

- The `#include` at the top of LibA.rc.
- The `1 TEXTINCLUDE` section in LibA.rc.
- Make sure that the back-reference from LibA_rc.h refers to the correct .rc file.

## Identifier collisions

Of course, the first thing that you'll want to do, having created your shiny new resource script is add some resources
to it.

This is a bad idea. The first thing you should do is bump up the ID values used by the library resource script so that
they don't collide with the application ID values.

Open `LibA_rc.h` and edit the `_APS_NEXT_RESOURCE_VALUE` line (and friends) as relevant. I'm not going to suggest a
sensible partitioning of the ID space. That's your problem.

Now, when you create a dialog or whatever, the resource editor will start from there and work upwards.

## Including the Resource Script in the main application

Having made sure that our static library builds OK, we need to make sure that the resources are available to the main
application.

Go to View / Resource Includes, and add your .h file to the symbol directives, and your .rc file to the compile-time
directives:

![](/images/2002/2002-07-31-resources-in-static-libs/resource_includes.png)

Now when you compile your application, the resources from the library will be included.

Hurrah!

Source code is available [here](/files/2002/2002-07-31-resources-in-static-libs/lib_resources_source.zip).
