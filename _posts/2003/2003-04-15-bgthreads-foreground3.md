---
title: "Using background threads for long processes in MFC: Foreground Thread 3"
date: 2003-04-15T11:43:39Z
short_title: "Foreground Thread 3"
tags: mfc
layout: series
series: mfc-background-threads
---

Because we have progress reporting, we can make use of a progress dialog to partially solve all of these problems. In
Visual C++, select "Project|Add To Project|Components and Controls".

Select "Progress Dialog" from "Visual C++ components". Accept the defaults, then derive the newly created class from
TaskObserver. Implement the OnProgress callback:

```c++
void CProgressDlg::OnProgress(int current, int maximum)
{
    m_Progress.SetRange(0, maximum);
    m_Progress.SetPos(current);

    PumpMessages();
}
```

We need the PumpMessages call, otherwise messages won't be processed, and this'll be no better than the other solutions.

This partially solves our problems:

- It repaints more frequently, when PumpMessages is called.
- The progress dialog can be moved, when PumpMessages is called.
- The user gets better feedback.

We still have problems:

- It still doesn't repaint often enough. Our demonstration app is fortunate, in that it reports progress fairly often.
  Consider a program where progress doesn't get reported that often.
- If the user starts to move the window around, the task doesn't get serviced. This could cause problems with timings,
  etc.

The source so far:  [bgthread_source1.zip](/files/2003/2003-04-15-bgthreads/bgthread_source1.zip).
