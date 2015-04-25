---
title: "Constructing a Medium-sized Project with Jam"
date: 2004-01-09T10:27:00.000Z
x-drupal-nid: 114
x-needs-review: 2004-01-09T10:27:00.000Z
---
## Introduction

Jam is a replacement for `make(1)`. See [here](http://www.perforce.com/jam/jam.html) for more details.

I'm attempting to use jam to build our Windows code, but in order to keep the scale of this discussion down, I'm just going to explain the aspects of our codebase that caused difficulty, and then I'm going to fake them up in a mock build tree. This will enable me to explain things in isolation.

## Introduction

*   [Building jam on Windows NT](/content/2004/01/building-jam-on-windows-nt).

## Tutorial

I've written this article in the form of a tutorial, because I think better when trying to formulate reproducible instructions for this kind of thing.

*   [Building an MFC program](/content/2001/07/building-an-mfc-application-with-jam-introduction).
*   [Building a shared library (DLL)](/drupal-4.7.3/tutorial/shared_lib/).
*   [Bringing it together with the SubDir rule](/drupal-4.7.3/tutorial/sub_dir/).
*   [Linking with a shared library](/drupal-4.7.3/tutorial/link_dll/).
*   [Building (and linking with) static libraries](/drupal-4.7.3/tutorial/static_lib/).
*   [Resource script dependencies](/content/2004/01/jam-resource-file-dependencies).
*   [Separate Debug/Release directories](/content/2002/02/jam-separate-release-debug-target-directories).

## Other Stuff

*   [Conflicting 'lib' target](/content/2004/01/conflicting-lib-target).
*   [Linker command line length](/content/2004/01/linker-command-line-length).

## Miscellaneous

This is some stuff I wrote earlier. I'm going to try to factor it into the main discussion, but for now, you can find it here:

*   [Linking with system libraries](/content/2004/07/jam-linking-with-system-libraries).