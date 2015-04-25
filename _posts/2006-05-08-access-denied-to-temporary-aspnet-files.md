---
title: "Access Denied to Temporary ASP.NET Files"
date: 2006-05-08T16:17:35.000Z
x-drupal-nid: 37
x-needs-review: 2006-05-08T16:17:35.000Z
---
Sometimes, when attempting to debug an ASP.NET Web Service, you'll get the following error: <tt>Access to the path "C:\WINDOWS\Microsoft.NET\Framework\v1.1.4322\Temporary ASP.NET Files\_somewhere_</tt>" is denied.

This can be caused when you're attempting to run with non-admin privileges and you've configured the ASP.NET worker process to run under [your account](/content/2006/02/debugging-asp-net-applications-without-administrator-privileges).

It can also be caused by the [Indexing Service](/content/2004/07/indexing-service-causes-strange-access-denied-errors-in-asp-net), so check that as well.

If you can't find another way to fix it, just make sure that the "VS Developers" group has full control over the <tt>Temporary ASP.NET Files</tt> directory.