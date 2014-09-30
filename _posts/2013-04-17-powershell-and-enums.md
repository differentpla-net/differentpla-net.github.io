---
title: PowerShell and enums
date: 2013-04-17T11:29:55Z
---
PowerShell has a really cool feature where it'll coerce a string into an `enum` value. I'll demonstrate by exporting a certificate from the Windows certificate store:

    PS> $cert = Get-Item Cert:\LocalMachine\My\597BB641C70A6B719126330D4DFE3455D11BDEFD

What parameters does `Export` take?

	PS> $cert.Export

	OverloadDefinitions
	-------------------
	byte[] Export(System.Security.Cryptography.X509Certificates.X509ContentType contentType)
	...two more...

You can see that the first overload expects an `X509ContentType` parameter, which is an enum:

	PS> [System.Security.Cryptography.X509Certificates.X509ContentType] | select BaseType

	BaseType
	--------
	System.Enum

...with the following members:

	PS> [Enum]::GetNames([System.Security.Cryptography.X509Certificates.X509ContentType])
	Unknown
	Cert
	SerializedCert
	Pfx
	Pkcs12
	SerializedStore
	Pkcs7
	Authenticode

So, we can call it with the actual enum value:

	PS> $bytes = $cert.Export([System.Security.Cryptography.X509Certificates.X509ContentType]::Pfx)

...or we can call it with a string, and rely on PowerShell to do the coercion:

	PS> $bytes = $cert.Export('Pfx')
