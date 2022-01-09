---
title: "Installing node.js, iisnode and npm on Windows"
date: 2012-01-19T14:21:51.000Z
---
I thought I might have a quick play with node.js on Windows. There's a bunch of conflicting information out there about how to install it. Here's what works for me.

1.  First, you'll need node.exe. Grab that from [http://nodejs.org/](http://nodejs.org/). Click the "Download" button. Install it, using the defaults; it'll go in `C:\Program Files (x86)\nodejs`.
2.  To run node on IIS, go to [https://github.com/windowsazure/iisnode/downloads](https://github.com/windowsazure/iisnode/downloads) and grab the appropriate build. For me, this was [iisnode-full-iis7-v0.1.16-x64.msi](https://github.com/downloads/WindowsAzure/iisnode/iisnode-full-iis7-v0.1.16-x64.msi). Install it.
3.  Check that your iisnode installation is working correctly by following the instructions [here](http://tomasz.janczuk.org/2011/08/hosting-nodejs-applications-in-iis-on.html).
4.  **npm** is now included in the nodejs installation.
5.  At this point, you should be able to run `npm install express` and good things should happen. Be sure to run this from your application folder. This is different from [the instructions given by Tomek](http://tomasz.janczuk.org/2011/08/hosting-express-nodejs-applications-in.html), which use ryppi.
6.  Verify that it's working by trying out Tomek's simple express application (same page as linked above).

Done. I'm still not sure what I'll use it for instead of my usual fumbling about with ASP.NET MVC, but now I'm down with the kids, right?
