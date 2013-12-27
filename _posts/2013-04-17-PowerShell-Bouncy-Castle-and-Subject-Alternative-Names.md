---
layout: post
title: "PowerShell, Bouncy Castle and Subject Alternative Names"
date: 2013-04-17T15:21:12.845Z
tags: powershell bouncy-castle certificates
alias: /post/UW69-yp74I4PAAAF/powershell-bouncy-castle-and-subject-alternative-names
---

Again, this is more-or-less a direct port from [the C# post](http://blog.differentpla.net/b/2013/21/24/bouncy-castle---subject-alternative-names).

    function Add-SubjectAlternativeName
	{
	[CmdletBinding()]
	param(
	    [Parameter(Mandatory = $true, ValueFromPipeline = $true)]
	    [Org.BouncyCastle.X509.X509V3CertificateGenerator] $CertificateGenerator,
	
	    [Parameter(Mandatory = $true)]
	    [string[]] $DnsName
	)
	
	    $names = $DnsName |
	        foreach {
	            New-Object Org.BouncyCastle.Asn1.X509.GeneralName(
	                [Org.BouncyCastle.Asn1.X509.GeneralName]::DnsName, $_)
	            }
	
	    $extension = New-Object Org.BouncyCastle.Asn1.DerSequence($names)
	
	    $CertificateGenerator.AddExtension(
	        [Org.BouncyCastle.Asn1.X509.X509Extensions]::SubjectAlternativeName.Id,
	        $false,
	        $extension)
	
	    return $CertificateGenerator
	}

Source code's in [the usual place](https://github.com/rlipscombe/PSBouncyCastle).