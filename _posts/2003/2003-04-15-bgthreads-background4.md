---
title: "Using background threads for long processes in MFC: Background Thread 4"
date: 2003-04-15T11:54:39Z
short_title: "Background Thread 4"
tags: mfc
layout: series
series: mfc-background-threads
---

The reason that our previous example consumes 100% CPU is that it busy-waits while pumping messages. The correct answer
is to run the dialog box modally. However, we can't (easily) use `DoModal`, because this causes ordering problems. Do we
create the thread or the dialog first?

However, MFC comes to our rescue with `RunModalLoop`:

```c++
void CBgthreadDlg::OnBackgroundModal()
{
    CProgressDlg dlg;
    dlg.Create();

    TaskThread *t = new TaskThread(&dlg);
    t->Start();

    dlg.RunModalLoop(0);

    delete t;

    dlg.DestroyWindow();
}
```

The only complication with this is that `RunModalLoop` doesn't return until the dialog calls `EndDialog`. In order to
cope with this, we have to add an `OnComplete` method to the observer, so that the dialog knows when the process is
complete.

The source code:  [bgthread_source4.zip](/files/2003/2003-04-15-bgthreads/bgthread_source4.zip).
