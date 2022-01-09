---
title: "Access Denied to Temporary ASP.NET Files"
date: 2006-05-08T16:17:35.000Z
---
Sometimes, when attempting to debug an ASP.NET Web Service, you'll get the following error:

```
Access to the path "C:\WINDOWS\Microsoft.NET\Framework\v1.1.4322\Temporary ASP.NET Files\...somewhere..." is denied.
```

This can be caused when you're attempting to run with non-admin privileges and you've configured the ASP.NET worker
process to run under [your account]({% post_url 2006/2006-02-20-debugging-aspnet-applications-without-administrator-privileges %}).

It can also be caused by the [Indexing Service]({% post_url 2004/2004-07-27-indexing-service-causes-strange-access-denied-errors-in-aspnet %}), so check that as well.

If you can't find another way to fix it, just make sure that the "VS Developers" group has full control over the
`Temporary ASP.NET Files` directory.
