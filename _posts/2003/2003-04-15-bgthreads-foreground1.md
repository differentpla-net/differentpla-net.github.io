---
title: "Using background threads for long processes in MFC: Foreground Thread 1"
date: 2003-04-15T11:41:39Z
short_title: "Foreground Thread 1"
tags: mfc
layout: series
series: mfc-background-threads
---

Initially, we'll run this as a foreground task. This will ensure that the code compiles and works before we get too
complicated. Also, it'll show why we want this task to run as a background thread.

To do this, add these files to the MFC dialog project, and add a button to the dialog. The code for the button should
look something like this:

```c++
void CBgthreadDlg::OnForeground()
{
    Task task(NULL);
    task.Run();
}
```

This runs our task on the foreground thread. This is the simplest solution. It demonstrates why we want our task to
run in the background:

- There's no feedback to the user that the application is busy.
- Try to move the dialog. You can't. Or, more accurately, it doesn't move until the task is finished, and control is
  returned to the main message loop.
- The window doesn't repaint until the task is finished.
