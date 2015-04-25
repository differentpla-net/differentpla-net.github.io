---
title: "Misleading Error: \"Parent class code is read only\" (Visual Studio.NET)"
date: 2004-01-29T21:09:00.000Z
x-drupal-nid: 60
x-needs-review: 2004-01-29T21:09:00.000Z
---
I just had a strange error message from Visual Studio.NET when attempting to add a handler for `WM_DESTROY` to a C++ dialog class. It said:

<pre>Add/Remove of the function is impossible because the parent class code is read only</pre>

Odd. There's nothing read-only about any of this stuff.

It turns out that I'd previously had an `OnDestroy` function in that class, and (somehow) the `ON_WM_DESTROY()` macro had been left in the message map. This apparently confuses Visual Studio.

Just remove the old `ON_WM_DESTROY()` line, and it's happy to add a new one.