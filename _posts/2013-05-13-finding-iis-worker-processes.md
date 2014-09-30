---
title: Finding IIS Worker Processes
date: 2013-05-13T13:37:46Z
---
How do we find out if there's a worker process for an application pool? Note that there can be more than one worker process for each application pool. Note also that a worker process can host multiple web applications.

In IIS 7.x, you can use `appcmd` (which is in `C:\Windows\System32\inetsrv`), as follows:

    > appcmd list wp
	WP "8828" (applicationPool:DefaultAppPool)

Get-WebAppPoolWorker
--

There doesn't seem to be a PowerShell equivalent for this, but we can do something like the following:

	function Get-WebAppPoolWorker() {
		& $appcmd list wp |
			% {
				$m = [regex]::Match($_, 'WP "(\d+)" \((.*)\)')
	
				$o = New-Object PSObject
				$processId = [int] $m.Groups[1].Value
	
				$o | Add-Member -MemberType NoteProperty -Name ProcessId -Value $processId
	
				# The output from 'appcmd list apppool' has (prop1:val1,prop2:val2),
				# so we'll just deal with that.
				$props = $m.Groups[2].Value
				$props.Split(',') | % {
					$prop = $_.Split(':')
					$o | Add-Member -MemberType NoteProperty -Name $prop[0] -Value $prop[1]
				}
	
				Write-Output $o
			}
	}
