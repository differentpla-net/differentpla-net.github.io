---
title: Failed to enumerate SSL bindings
date: 2013-11-11T14:53:04Z
redirect_from: /post/36
tags: powershell certificates
---
In PowerShell, when I attempted to enumerate SSL bindings, I was getting the
error message:

```
Failed to enumerate SSL bindings, error code 234.
```

This is sometimes due to misconfiguration of the SSL certificate bindings in the registry, under
`HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\services\HTTP\Parameters\SslBindingInfo`. This controls the SSL certificate
bindings; either all of the entries in here must have a value named `SslCertStoreName`, or none of them must.

One of my colleagues found this answer on [this MSDN forum](http://social.technet.microsoft.com/Forums/windowsserver/en-US/87b1252d-a6a0-4251-bbb6-38e104a8c07a/enumerating-iissslbindings-gives-failure-on-one-machine-works-on-another?forum=winserverpowershell#0425cd3a-0da0-45df-960f-a614bf30aae1), but that's kinda hard to read.

With PowerShell:

```powershell
# Find those entries which are missing the 'SslCertStoreName' property:
# Note, that, for some reason, this includes the `[::1]:443` entry, even if it's not broken,
# which means that we can't easily script the fix.
$brokenBindings = dir HKLM:\SYSTEM\CurrentControlSet\services\HTTP\Parameters\SslBindingInfo |
        where { !(Get-ItemProperty -Path $_.PSPath -Name SslCertStoreName) }

# Output the certificate thumbprint for the broken ones:
$brokenBindings | % {
                $hashBytes = (Get-ItemProperty -Path $_.PSPath -Name SslCertHash).SslCertHash
                $hashString = [BitConverter]::ToString($hashBytes).Replace('-', '')
                Write-Output ("{0} -- {1}" -f $_.Name, $hashString)
}
```
