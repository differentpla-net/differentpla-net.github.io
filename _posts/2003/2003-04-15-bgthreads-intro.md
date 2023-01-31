---
title: "Using background threads for long processes in MFC"
date: 2003-04-15T11:20:39Z
short_title: "Introduction"
tags: mfc
layout: series
series: mfc-background-threads
---

It's not easy to write a Windows program that takes a long time to do something and yet still seems responsive to the
user. Consider that you've got a single task that needs to be finished:

- The task is going to take too long to simply use an hourglass.
- Progress reporting (percentages, for example) is relatively infrequent.

This article will lay out a couple of different ways to solve this problem.  It's broken down into reasonably-sized
sections:

{% include _series_toc.html %}

## Test Harness

We'll start with a test program.  The simplest thing to do is to fire up Visual C++ and generate a brand-new MFC dialog
application.  We can then put buttons on it, so that we can experiment with different ways to schedule the task.
