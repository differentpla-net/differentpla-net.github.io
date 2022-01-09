---
title: "PowerShell: ImportSystemModules"
date: 2012-01-05T13:58:41.000Z
---
I've been using PowerShell as a better Command Prompt for a while now. Increasingly, however, it's one of the first tools I reach for when I come across a problem, rather than being an afterthought.

I've been tweaking my profile to pre-load a bunch of modules. I've got the [PowerShell Community Extensions](http://pscx.codeplex.com/); I've got [PsGet](http://psget.net/); I've got [Posh-Hg](http://poshhg.codeplex.com/).

One of the things that caught my eye was the "Import system modules" entry on the PowerShell jump list.

This runs `Powershell.exe -ImportSystemModules`, which, in turn, runs the ImportSystemModules command. You can call this command as part of your profile, if you want these modules loaded each time you run PowerShell.

Various places on the Internet state that it loads the available modules from `C:\Windows\System32\WindowsPowerShell\v1.0\Modules`. Among these are modules for managing IIS, Hyper-V, etc.

It turns out that it also loads snap-ins from `C:\Users\rogerl\Documents\WindowsPowerShell\Snapins`, as you can see from running the following:

<pre>PS> (Get-Command importsystemmodules).Definition
$SnapIns = @(Get-PSSnapin -Registered -ErrorAction SilentlyContinue)
$Modules = @(Get-Module -ListAvailable -ErrorAction SilentlyContinue |
    ? { $_.ModuleBase -like "$pshome*" })
# etc....
</pre>
