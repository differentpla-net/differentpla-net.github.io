---
title: "How do I debug my gadget?"
date: 2007-04-24T10:34:09.000Z
tags: sidebar-gadgets
---
As far as I can tell, you'll have to go "old school", and do the equivalent of adding lots of `printf` statements to
your code. This should help:

```javascript
function outputDebug(message) {
    var fso = new ActiveXObject("Scripting.FileSystemObject");
    var f = fso.OpenTextFile("C:\\Temp\\Gadget.log", 8, true);
    f.WriteLine(message);
    f.Close();
}
```

**Update:** I figured out [how to use Visual Studio]({% post_url 2007/2007-04-30-debugging-sidebar-gadgets %}) to debug sidebar gadgets.
