---
title: "Using background threads for long processes in MFC: Background Thread 3"
date: 2003-04-15T11:53:39Z
short_title: "Background Thread 3"
tags: mfc
layout: series
series: mfc-background-threads
---

One simple way to run the task in the background, while reporting progress, which still allows the dialog to be
repainted/moved correctly is to busy-wait for the background thread to finish, while pumping messages:

```c++
void CBgthreadDlg::OnBackgroundBusy()
{
    CProgressDlg dlg;
    dlg.Create();

    TaskThread *t = new TaskThread(&dlg);
    t->Start();

    // Wait for it to finish, but pump messages in the meantime.
    while (t->GetExitStatus() == STILL_ACTIVE)
    {
        dlg.PumpMessages();
        Sleep(0);
    }

    delete t;

    dlg.DestroyWindow();
}
```

This works fine, but for the problem that it uses 100% CPU while busy-waiting. This wastes CPU power, because it has to
keep context-switching, and it'll ruin battery life on a laptop, because the CPU is running continuously.
