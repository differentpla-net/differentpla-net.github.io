---
title: "How do I debug my gadget?"
date: 2007-04-24T10:34:09.000Z
x-drupal-nid: 175
x-needs-review: 2007-04-24T10:34:09.000Z
---
As far as I can tell, you'll have to go "old school", and do the equivalent of adding lots of <tt>printf</tt> statements to your code. This should help:

<pre>function outputDebug(message) {
    var fso = new ActiveXObject("Scripting.FileSystemObject");
    var f = fso.OpenTextFile("C:\\Temp\\Gadget.log", 8, true);
    f.WriteLine(message);
    f.Close();
}</pre>

**Update:** I figured out [how to use Visual Studio](/content/2007/04/debugging-sidebar-gadgets) to debug sidebar gadgets.
