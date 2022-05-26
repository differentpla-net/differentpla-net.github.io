---
title: "Jam: Linking with System Libraries"
date: 2004-07-04T07:00:00.000Z
tags: jam
---

Obviously, your code doesn't just link with your libraries. It also has to link with some of the system libraries. Jam manages this by using the `LINKLIBS` variable. The simplest way to make this work is something like the following:

```
    LINKLIBS on emplode.exe += ws2_32.lib ;
```

Here you can see that we're telling jam to pass `ws2_32.lib` on to the linker when it tries to link `emplode.exe`.

There are two problems with this approach:

- It's a bit Windows-specific.
- The Jamfile needs to know that the target is called `emplode.exe`. It hasn't yet had to know.
