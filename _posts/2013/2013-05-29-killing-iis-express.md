---
title: Killing IIS Express
date: 2013-05-29T17:23:35Z
layout: series
series: visual-studio-extension-in-a-weekend
---
A brief aside: I've got to the point where I'm about to fire up IIS Express from Visual Studio. In order to be a good
citizen, our extension should probably shut it down again later.

So, let's fire up JetBrains dotPeek and see how the IIS Express tray application does it.

The obvious place to start looking is in `IisExpressWorkerProcess.Stop`. This calls
`IisExpressHelper.SendStopMessageToProcess`, passing a process ID.

This searches using `GetTopWindow` and `GetWindow(GW_NEXT)` for top-level windows belonging to the correct process. It
then posts a `WM_QUIT` message to it. I'll add some code for that later.

## Why not use Process.CloseMainWindow?

`Process.CloseMainWindow()` posts a `WM_CLOSE` message, rather than `WM_QUIT`. But, mainly, it doesn't work with IIS
Express, because `MainWindowHandle` is zero.
