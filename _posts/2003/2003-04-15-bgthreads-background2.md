---
title: "Using background threads for long processes in MFC: Background Thread 2"
date: 2003-04-15T11:52:39Z
short_title: "Background Thread 2"
tags: mfc
layout: series
series: mfc-background-threads
---

We can solve the problem of the memory leak, and of the premature destructor call, by simply blocking until the thread
is finished:

```c++
void CBgthreadDlg::OnBackgroundJoin()
{
    CWaitCursor wait;

    TaskThread *t = new TaskThread;
    t->Start();

    // Wait for it to finish
    t->Join();
    delete t;
}
```

This is, in effect, no better than any of our foreground thread examples, but it does get us a step closer to the ideal
solution.

The source so far:  [bgthread_source3.zip](/files/2003/2003-04-15-bgthreads/bgthread_source3.zip).
