---
title: "Indexing Service causes strange Access Denied errors in ASP.NET"
date: 2004-07-27T18:14:00.000Z
x-drupal-nid: 39
x-needs-review: 2004-07-27T18:14:00.000Z
---
A couple of weeks ago, I installed a trial copy of the Dundas .NET charting controls to play with in ASP.NET. It's actually pretty cool, but I was attempting to show it to someone on my laptop when it stopped working.

I kept getting error messages about "Access Denied" and pointing at an unrelated line in machine.config.

It turns out that, contrary to my cursing, it wasn't Dundas' fault. I got the same message when attempting to use one of my own class libraries just now.

It's a known problem, and is discussed in the Microsoft Knowledge Base under the snappy title of [PRB: Access Denied Error When You Make Code Modifications with Index Services Running](http://support.microsoft.com/default.aspx?scid=kb;en-us;329065).

So, if you get this error, turn off Indexing Service. Alternatively, if you're actually using it, stop it from poking around in the "Temporary ASP.NET Files" directory.