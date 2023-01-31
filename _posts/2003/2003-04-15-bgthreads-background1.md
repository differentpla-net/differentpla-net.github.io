---
title: "Using background threads for long processes in MFC: Background Thread 1"
date: 2003-04-15T11:51:39Z
short_title: "Background Thread 1"
tags: mfc
layout: series
series: mfc-background-threads
---

We add another button to the dialog:

```c++
void CBgthreadDlg::OnBackground()
{
    // Run on a background thread. Don't wait for the thread to exit.
    TaskThread t;
    t.Start();
}
```

This uses the code from <a href="http://www/~roger/devel/background_threads/cpp_thread.html">here</a> to implement the
thread class:

```c++
class TaskThread : public Thread
{
public:
    virtual unsigned Run()
    {
        Task task(NULL);
        task.Run();

        return 0;
    }
};
```

The source so far: [bgthread_source2.zip](/files/2003/2003-04-15-bgthreads/bgthread_source2.zip).

This works fine: the task is run on the background thread, making the dialog totally reponsive to the user. It has
several of its own problems:

- There's no indication that it's doing anything -- we want to report progress to the user, but still have a responsive
  application.
- The user can spawn multiple background threads. Depending on your application, this might be fine. We'll assume for
  the moment that it's not.
- If the user closes the dialog, the application terminates the background thread with extreme prejudice.

More than that, however, there's a problem with stability:  About 80% of the time, the thread fails to start, failing
with an access violation or a pure virtual function call.

This is caused by the destructor for TaskThread being called from the `CBgthreadDlg::OnBackground()` function before the
background thread has had a chance to call the virtual `Run` function from `StaticThreadRoutine`.

We can fix this by allocating the thread object on the heap, rather than the stack. It's then necessary to clean up the
thread object properly, otherwise we get a memory leak. We'll deal with this in the next post.
