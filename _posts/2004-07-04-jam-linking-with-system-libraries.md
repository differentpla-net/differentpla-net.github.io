---
title: "Jam: Linking with System Libraries"
date: 2004-07-04T07:00:00.000Z
x-drupal-nid: 122
x-needs-review: 2004-07-04T07:00:00.000Z
---
# System Libraries

Obviously, your code doesn't just link with your libraries. It also has to link with some of the system libraries. Jam manages this by using the `LINKLIBS` variable. The simplest way to make this work is something like the following:

<div class="before">
<pre>LINKLIBS on emplode.exe += ws2_32.lib ;
</pre>

</div>

Here you can see that we're telling jam to pass `ws2_32.lib` on to the linker when it tries to link `emplode.exe`.
The main problem with this approach is that it's a bit Windows-specific. If we put aside the fact that we know we've got to link with `ws2_32.lib` for a moment, we still can't ignore the fact that the Jamfile needs to know that the target is called `emplode.exe`. It hasn't yet had to know.