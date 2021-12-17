---
title: IIS Application Pools and Auto-Started Web Applications
date: 2013-05-13T13:38:19Z
---
We had an issue at work last week where a web application was taking a little bit too long to start up, which was causing intermittent failures. We'd like the application to be always-running, so that this doesn't happen.

This led to an investigation of the vagaries of IIS application pool configuration.

What does it mean when an application pool is displayed as 'Started'?
--

In IIS Manager, under "Application Pools", you'll see in the **Status** column that your application pools are shown as "Started". This doesn't mean that there's a worker application ready to accept requests (the value should probably be "Ready").

The `autoStart` attribute
--

In the [applicationpools/add](http://www.iis.net/configreference/system.applicationhost/applicationpools/add) node in `applicationHost.config`, there are two settings which might be of interest. The `autoStart` attribute corresponds to the "Start Automatically" property shown in the "Advanced Settings" dialog for the application pool.

This doesn't actually *start* the application pool; it's equivalent to the "Started" status discussed above. It (more-or-less) enables the application pool, so that it's ready when IIS starts up.

The `startMode` attribute
--

The second interesting attribute in [applicationpools/add](http://www.iis.net/configreference/system.applicationhost/applicationpools/add) is the `startMode` attribute. This doesn't appear to be available in the IIS console, but you can set it from PowerShell:

    $pool = Get-Item 'IIS:\AppPools\DefaultAppPool'
	$pool.startMode = 'AlwaysRunning'
	$pool | Set-Item

Idle Timeout
--

IIS stops web worker processes when they're idle, in order to save resources. This may cause problems if your application takes a while to start up, so we want to disable this as well:

	$pool = Get-Item 'IIS:\AppPools\DefaultAppPool'
	$pool.processModel.idleTimeout = [TimeSpan]::Zero
	$pool | Set-Item

Also see ["Configure Idle Time-out Settings for an Application Pool (IIS 7)"](http://technet.microsoft.com/en-us/library/cc771956.aspx)

Configuring recycling
--

The other problem that we had was that IIS [recycles](http://www.iis.net/configreference/system.applicationhost/applicationpools/add/recycling/periodicrestart) the application pool every [29 hours](http://serverfault.com/questions/348493/why-does-the-iis-worker-process-recycle-every-29-hours-and-not-every-24-hours). We'd rather it recycled outside working hours, at a fixed time each day.

So, using the details from [here](http://serverfault.com/a/441972/7027):

	$pool = Get-Item 'IIS:\AppPools\DefaultAppPool'
	$pool.recycling.periodicRestart.time = [TimeSpan]::Zero
	$pool | Set-Item

	Clear-ItemProperty "IIS:\AppPools\$($pool.Name)" -Name recycling.periodicRestart.schedule
	Set-ItemProperty "IIS:\AppPools\$($pool.Name)" `
		-Name recycling.periodicRestart.schedule -Value @{value='02:00:00'}

...we can set the application pool to recycle at 2am.

Other Links
--

* [Launch worker process (w3wp.exe) automatically as soon as application pool starts](http://blogs.msdn.com/b/amb/archive/2012/03/08/launch-worker-process-w3wp-exe-automatically-as-soon-as-application-pool-starts.aspx)
* [Auto-Start ASP.NET Applications (VS 2010 and .NET 4.0 Series)](http://weblogs.asp.net/scottgu/archive/2009/09/15/auto-start-asp-net-applications-vs-2010-and-net-4-0-series.aspx)
