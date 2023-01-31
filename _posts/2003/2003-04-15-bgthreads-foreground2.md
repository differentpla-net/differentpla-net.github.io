---
title: "Using background threads for long processes in MFC: Foreground Thread 2"
date: 2003-04-15T11:42:39Z
short_title: "Foreground Thread 2"
tags: mfc
layout: series
series: mfc-background-threads
---

We can deal with the first one (no feedback), by using an hourglass:

```c++
void CBgthreadDlg::OnForegroundHourglass()
{
    CWaitCursor wait;

    Task task(NULL);
    task.Run();
}
```

This certainly looks better. The user knows that the application is busy, so they're less likely to think it's gone
wrong. It still doesn't repaint properly, though.
