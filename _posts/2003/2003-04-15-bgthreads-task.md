---
title: "Using background threads for long processes in MFC: Sidebar: The Task"
date: 2003-04-15T11:30:39Z
short_title: "Sidebar: The Task"
tags: mfc
layout: series
series: mfc-background-threads
---

```c++
// task.h
class TaskObserver;

class Task
{
    TaskObserver *m_pObserver;

public:
    Task(TaskObserver *pObserver)
        : m_pObserver(pObserver) { }

    void Run();

private:
    inline void ReportProgress(int current, int maximum);
};

class TaskObserver
{
public:
    virtual void OnProgress(int current, int maximum) = 0;
};

inline void Task::ReportProgress(int current, int maximum)
{
    if (m_pObserver)
        m_pObserver->OnProgress(current, maximum);
}
```

```c++
// task.cpp
#include "StdAfx.h"
#include "task.h"

void Task::Run()
{
    // Pretend to do some work.
    const int MAX_ITERATIONS = 10;
    for (int i = 0; i < MAX_ITERATIONS; ++i)
    {
        ReportProgress(i, MAX_ITERATIONS);
        Sleep(1000);
    }

    ReportProgress(MAX_ITERATIONS, MAX_ITERATIONS);
}
```

Essentially, this gives us a task that we can run.  It demonstrates two things -- the longevity of the task, and the
fact that it reports its progress reasonably infrequently.
