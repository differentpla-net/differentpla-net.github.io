---
title: "SQL Server Management Studio: Unable to read the list of previously registered servers on this system"
date: 2012-04-03T13:28:41.000Z
redirect_from: /content/2012/04/sql-server-management-studio-unable-read-list-previously-registered-servers-system
---
I recently started having odd problems with SQL Server Management Studio on my work PC.

When I'd run it, I got the following exception, and I couldn't browse any SQL server instances.

```
Microsoft.SqlServer.Management.RegisteredServers.RegisteredServerException:
Unable to read the list of previously registered servers on this system.
Re-register your servers in the 'Registered Servers' window.

---> Microsoft.SqlServer.Management.Sdk.Sfc.SfcSerializationException:
Deserialization operation on /RegisteredServersStore/ServerGroup/CentralManagementServerGroup has failed.

---> System.InvalidOperationException: Unable to generate a temporary class (result=1).
error CS2001: Source file 'C:\Windows\TEMP\gb_pz65v.0.cs' could not be found
error CS2008: No inputs specified
```

To fix this exception, I deleted my `%APPDATA%\Microsoft\Microsoft SQL Server` folder. When I ran SQL Server Management Studio again, this exception was gone, but I had to re-register all of my servers.

Unfortunately, after that I get another exception:

```
Value cannot be null.
Parameter name: viewInfo (Microsoft.SqlServer.Management.SqlStudio.Explorer)
```

However, at the same time, Visual Studio 2010 (well, actually ReSharper) started reporting the following:

```
Unable to generate a temporary class (result=1). error CS2001: Source file 'C:\Windows\TEMP\oaspyffw.0.cs' could not be found error CS2008: No inputs specified
```

...which is almost identical to the error that I was seeing from SSMS.

It appears that my %TEMP% environment variable is set incorrectly. If, on this machine, I go to Start / Run and enter `%TEMP%`, I get a UAC prompt. It looks like the variable's pointing to `C:\Windows\Temp`.

However, I can't figure out _where_ it's set. The variable looks fine in PowerShell, but not in Command Prompt.

Light-bulb moment. Rebooting to check something.

Back from rebooting.

A couple of days ago, I "cleaned out" my `%APPDATA%\Temp` folder. It seems that I deleted a bit too much. Recreating the `2` subdirectory and rebooting, everything seems fine.

The clue was on the [MS Connect site](http://connect.microsoft.com/SQLServer/feedback/details/573771/value-cannot-be-null).

There's another clue as to how this might have happened on the [MSDN Forums](http://social.msdn.microsoft.com/Forums/en-US/sqltools/thread/236d9263-a67e-4926-a7a1-b1173553b8aa/), of which more [here](http://technet.microsoft.com/en-us/library/cc755098.aspx).

**Update 2018-12-30 (from memory): I nuked the machine from orbit. It was the only way to be sure.**
