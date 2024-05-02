---
title: MSBuild - where is everything?
date: 2013-11-11T14:47:09Z
tags: msbuild
---
MSBuild (and Visual Studio) scatter `.targets` and `.tasks` files all over the shop. Try looking in:

* `C:\Windows\Microsoft.NET\Framework64\v4.0.30319\...`
* `C:\Program Files\MSBuild\...`
* `C:\Program Files (x86)\MSBuild\...`

In PowerShell, something like this would work:

```powershell
$msbuildPaths = 'C:\Windows\Microsoft.NET\Framework64\v4.0.30319',
        'C:\Program Files\MSBuild',
        'C:\Program Files (x86)\MSBuild'

$msbuildFiles = Get-ChildItem -Path $msbuildPaths -Recurse -Include *.targets,*.tasks
```

Then you can search in those files as follows:

```powershell
$msbuildFiles | Select-String 'CodeAnalysisInputAssembly'
```
