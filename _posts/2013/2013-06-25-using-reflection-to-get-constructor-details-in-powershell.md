---
title: Using Reflection to get constructor details in PowerShell
date: 2013-06-25T18:54:25Z
tags: powershell
redirect_from:
  - /b/2013/51/25/using-reflection-to-get-constructor-details-in-powershell
---
I was attempting to convert some Java code that uses Bouncy Castle into the
equivalent PowerShell. The Java code was using the `PKCS10CertificationRequest` class,
and I needed to see if there was an equivalent in the .NET libraries:

```powershell
Add-Type -Path .\packages\BouncyCastle.1.7.0\lib\Net40-Client\BouncyCastle.Crypto.dll

$assembly = [AppDomain]::CurrentDomain.GetAssemblies() |
	where { $_.GetName().Name -eq 'BouncyCastle.Crypto' }

$assembly.GetExportedTypes() |
	where { $_.Name -like '*CertificationRequest*' } |
	% { $_.FullName }

[Org.BouncyCastle.Pkcs.Pkcs10CertificationRequest].GetConstructors() |
	% { $_.ToString() }
```

This gives us (reformatted slightly):

	Void .ctor(Byte[])
	Void .ctor(Org.BouncyCastle.Asn1.Asn1Sequence)
	Void .ctor(System.IO.Stream)
	Void .ctor(System.String,
		Org.BouncyCastle.Asn1.X509.X509Name,
		Org.BouncyCastle.Crypto.AsymmetricKeyParameter,
		Org.BouncyCastle.Asn1.Asn1Set,
		Org.BouncyCastle.Crypto.AsymmetricKeyParameter)

So it looks like we want the last one, because it's got more parameters. Can we get the parameter names?

	$ctor = [Org.BouncyCastle.Pkcs.Pkcs10CertificationRequest].GetConstructors() |
		select -Last 1
	$ctor.GetParameters() | select ParameterType, Name

Cool:

	ParameterType                                               Name
	-------------                                               ----
	System.String                                               signatureAlgorithm
	Org.BouncyCastle.Asn1.X509.X509Name                         subject
	Org.BouncyCastle.Crypto.AsymmetricKeyParameter              publicKey
	Org.BouncyCastle.Asn1.Asn1Set                               attributes
	Org.BouncyCastle.Crypto.AsymmetricKeyParameter              signingKey
