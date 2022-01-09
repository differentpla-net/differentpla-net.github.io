---
title: "Shell_NotifyIcon: NIM_SETVERSION should be sent after NIM_ADD"
date: 2007-09-30T22:33:25.000Z
---
Like it says: NIM_SETVERSION always returns FALSE unless you've used NIM_ADD. So send NIM_ADD first.