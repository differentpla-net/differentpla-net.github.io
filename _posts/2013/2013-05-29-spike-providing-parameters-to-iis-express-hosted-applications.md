---
title: "Spike: Providing parameters to IIS Express-hosted applications"
date: 2013-05-29T15:32:53Z
---
An aside: We need to be able to support multiple instances of our small web 
application (because you might have more than one instance of Visual Studio open).
If we host our application in IIS Express (and remember that we're doing this to
avoid messing around with URL ACLs), we either need to duplicate the whole thing 
for each site, or figure out a way to provide it with parameters.

My first thought was that Visual Studio adds a new `<site>` element to the IIS 
Express `applicationHost.config` file for each web application that you choose to
use IIS Express for. Maybe we could add something to this `<site>` element?

It turns out that: no, it doesn't appear that we can -- the
[schema](http://www.iis.net/configreference/system.applicationhost/sites/site/application)
doesn't allow for configuration settings.

OK, so maybe we can clone the entire `applicationHost.config` file? IIS Express
actually does this if you use the `/path` parameter on the command line
(you can see this in the console output when running the command `iisexpress.exe`).
Then we can point IIS Express at a custom configuration file, and our application
can figure out its settings from there.

Even if we do this, it doesn't appear that we can easily load sections of this file.

I also just tried a quick spike to see if we could use `WebConfigurationManager`
to get `ApplicationSettings` from the `applicationHost.config` file. No luck.

It looks like we'll just have to clone the whole web application folder.
