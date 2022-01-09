---
title: "Getting NBehave to work with NUnit 2.5.5"
date: 2010-05-20T14:37:51.000Z
---
NBehave depends on NUnit 2.5.2. To get it to work with NUnit 2.5.5, you can create an NBehave-Console.exe.config file containing the following:

```xml
<? xml version="1.0" encoding="utf-8" ?>
<configuration>
  <runtime>
    <assemblyBinding xmlns="urn:schemas-microsoft-com:asm.v1">
      <dependentAssembly>
        <assemblyIdentity name="nunit.framework"
                          culture="neutral"
                          publicKeyToken="96d09a1eb7f44a77" />
        <bindingRedirect oldVersion="2.5.2.9222" newVersion="2.5.5.10112" />
      </dependentAssembly>
    </assemblyBinding>
  </runtime>
</configuration>
```

This will cause it to use 2.5.5.10112 instead of 2.5.2.9222\. You’ll also need to overwrite the copy of nunit.framework in the NBehave directory with the newer version.
