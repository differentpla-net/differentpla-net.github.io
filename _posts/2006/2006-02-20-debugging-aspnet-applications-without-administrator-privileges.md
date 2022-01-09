---
title: "Debugging ASP.NET applications without Administrator privileges"
date: 2006-02-20T11:32:53.000Z
redirect_from: /content/2006/02/debugging-asp-net-applications-without-administrator-privileges
---
I recently had my machine at work rebuilt with Windows 2003, and had a spot of difficulty getting Visual Studio to debug my ASP.NET applications. Here are the various error messages I encountered, and their fixes.

The first thing you need to check is that your user account is a member of the `VS Developers` group. Fortunately, Visual Studio is usually pretty good about telling you this.

<pre>Error while trying to run project: Unable to start debugging on the
web server. The project is not configured to be debugged.</pre>

First, verify that your Web.config has debug="true". This should be in the following location: `<configuration> <system.web> <compilation debug="true" ... /> ...`

If this is set, you should check that your virtual directory is configured to run as an application.

To do this, open IIS Manager and bring up the properties for the virtual directory. On the 'Virtual Directory' page, under 'Application settings', click the 'Create' button.

You also need to set 'Execute permissions' to 'Scripts only'.

<pre>Error while trying to run project: Unable to start debugging on the
web server. Access is denied.</pre>

<pre>Error while trying to run project: Unable to start debugging on the
web server. Could not start ASP.NET or ATL Server debugging.
Verify that ASP.NET or ATL Server is correctly installed on the server.</pre>

In order to debug a web application, you need to either be an Administrator, or the application should be running under your user account.

Since we're trying to do our development without Administrator privileges, you'll configure the ASP.NET worker process to run under your user account.

[Windows XP only]

On Windows XP, this is done by editing the machine.config file. This file is stored in C:\WINDOWS\Microsoft.NET\v1.1.4322\CONFIG.

Find the <processModel> node, and set the userName and password attributes to the appropriate values for your user account.

You might need to run iisreset to get the settings to take.

[Windows 2003 only]

On Windows 2003, the identity of the worker process depends on which application pool it's configured to run under, and which account is being used by that application pool.

In IIS Manager, go to Application Pools and create a new application pool. Call it DebugAppPool, and use the default settings. Bring up the properties dialog. On the 'Identity' tab, configure it to run using your user account details.

Configure each of your ASP.NET applications to run in this application pool.

This is important: if you configure DefaultAppPool to run as your identity, various other web applications (like the Microsoft Virtual Server admin console) might stop working. They're expecting to run with NetworkService credentials.

Again, you might need to run iisreset to get the settings to take.

<pre>The Web server reported the following error when attempting to create
or open the Web project located at the following URL: 'url'.
'HTTP/1.1 503 Service Unavailable'.</pre>

In the context of attempting to debug as a non-Admin user, this usually means that the application pool configured for your web app has been disabled.

Run Event Viewer and look in the System log. You'll see a couple of warnings and an error from W3SVC.

The first warning contains the following text:

<pre>The identity of application pool, 'DefaultAppPool' is invalid.  If it
remains invalid when the first request for the application pool is
processed, the application pool will be disabled.
The data field contains the error number.</pre>

You can look up the error code from the data field. Bear in mind that it's a little-endian number, so it'll be backwards. For example, if it contains 69 05 07 80, then this is error 0x80070569.

Look this error up in the Error Lookup tool (it's on the Tools menu in Visual Studio .NET 2003).

This particular error translates as the following:

<pre>Logon failure: the user has not been granted the requested logon type
at this computer.</pre>

To fix this, add your user account to the IIS_WPG group. You'll need to run iisreset.

At this point, ASP.NET debugging started working for me.
