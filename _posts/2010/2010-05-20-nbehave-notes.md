---
title: "NBehave Notes"
date: 2010-05-20T08:55:37.000Z
---
We’ve just started using NBehave, so this is a random selection of notes:

If you try to use NBehave-Console with a .NET 4.0-compiled assembly, you’ll see a BadImageFormatException. You can fix this by creating the NBehave-Console.exe.config file, as follows:

```xml
<?xml version="1.0" encoding="utf-8" ?>
<configuration>
  <startup>
    <supportedRuntime version="v4.0.30319"/>
  </startup>
</configuration>
```

This makes it use the v4.0 CLR. See [this question on StackOverflow](http://stackoverflow.com/questions/2046089/force-an-application-to-run-under-specific-net-runtime-version).

NBehave’s command-line parsing is a bit flakey. If you pass /scenarioFile (rather than /scenarioFiles), the invalid switch is **ignored**; no error is raised.

Similarly, it ignores unrecognised lines in the text scenario files, so make sure that you type “When”, “Given”, etc., correctly.

Also, NBehave-Console.exe is compiled for “Any CPU”, so if you’re testing stuff that requires 32-bit DLLs (e.g. COM components), you’ll need to use CORFLAGS /32BIT+ to fix up the EXE.
