---
title: Running IIS Express
date: 2013-05-29T15:07:25Z
---
When you create a web application in Visual Studio, you can configure it (on the
properties page) to run in **IIS Express**.

I made use of it in my "Visual Studio Extension in a Weekend" project. The
advantages of using IIS Express for hosting the rendering application part of that project are:

1. It's installed as part of Visual Studio, so I can pretty much assume that it's available.
2. It runs happily without administrator privileges, which is nice.

###Spike: how to run IIS Express?###

In `C:\Program Files (x86)\IIS Express`, you can find the `iisexpress.exe` 
application. It will display help text if you run it with the `/?` switch.

You can run it with the `/path` switch, and it will start up a web server instance.

If you run it from a command prompt, it runs as a console application, which is 
a bit ugly.

If you start a web application in Visual Studio, IIS Express runs in the 
notification area, with no ugly console window. What's the difference? 
With IIS Express running in this mode, Task Manager shows two applications: 
"IIS Express System Tray" (iisexpresstray.exe) and "IIS Express Worker Process" 
(iisexpress.exe).

Showing the "command line" column in Task Manager shows that `iisexpress.exe` 
is run with the following arguments:

    /config:"C:\Users\name\Documents\IISExpress\config\applicationhost.config"
    /site:"MvcApplication1"
    /apppool:"Clr4IntegratedAppPool"`

And `iisexpresstray.exe` is run passing the process ID of the `iisexpress.exe` 
process.

However, if we try this ourselves, we still get an ugly console window.

Process Explorer
--

I'm guessing here, but I suspect that Visual Studio simply starts up the process
hidden. I'm going to use Process Explorer to see if this is the case.

The first thing that Visual Studio does is look in the registry (under 
`HKLM\SOFTWARE\Wow6432Node\Microsoft\IISExpress\8.0\InstallPath`) to see where 
IIS Express is installed. Oddly, it seems to look for IIS Express 7.5 first.

It also queries the `Manifest` registry value, and then opens the file named here. 
However, on looking in this file, there doesn't appear to be anything interesting, 
so I don't know why it's doing this. I'll ignore it for now.

Unfortunately, it doesn't appear that Process Explorer allows you to see the 
exact parameters passed to `CreateProcessW`, so it's either time to break out 
WinDbg, or time to just try it.

So: let's just try it. Time for a bit of PowerShell.

Where's PowerShell ISE gone?
--

A brief interlude ensues here: where's PowerShell ISE on Windows 8? It turns out 
that it is installed, but it's not shown on the Start screen by default. 
See [this](http://trekker.net/archives/where-is-the-powershell-ise-in-windows-8/) 
for more details.

After a couple of minutes of messing about, I came up with the following:

    $applicationRoot = 'C:\path\to\webapp'
	
	$iisexpress = Join-Path `
	    (Get-ItemProperty 'HKLM:\SOFTWARE\Wow6432Node\Microsoft\IISExpress\8.0' 'InstallPath').InstallPath `
	    'iisexpress.exe'
	
	$startInfo = New-Object System.Diagnostics.ProcessStartInfo
	$startInfo.FileName = $iisexpress
	$startInfo.Arguments = ('/path:"{0}"' -f $applicationRoot)
	$startInfo.CreateNoWindow = $true
	$startInfo.WindowStyle = 'Hidden'
	
	$p = [System.Diagnostics.Process]::Start($startInfo)

...which works beautifully, but it turns out that it's even simpler:

	Start-Process $iisexpress ('/path:"{0}"' -f $applicationRoot) -WindowStyle Hidden

It also turns out that IIS Express starts the tray icon by itself, so the whole
thing can be done with one line, and we can start the default web browser with one more:

	Start-Process 'http://localhost:8080'
