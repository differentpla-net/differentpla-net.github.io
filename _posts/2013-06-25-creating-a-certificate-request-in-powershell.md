---
title: Creating a Certificate Request in PowerShell
date: 2013-06-25T18:56:28Z
---
The [documentation](http://www.bouncycastle.org/wiki/display/JA1/X.509+Public+Key+Certificate+and+Certification+Request+Generation#X.509PublicKeyCertificateandCertificationRequestGeneration-CreatingCertificationRequests) is for Java, and mentions the `PKCS10CertificationRequest` class.

The equivalent in the .NET libraries is `Org.BouncyCastle.Pkcs.Pkcs10CertificationRequest`.

Creating a simple certificate request
-------------------------------------

Let's give it a go:

    $signatureAlgorithm = 'SHA256WithRSA'
	$subjectDN = New-X509Name 'CN=Whatever'
	$keyPair = New-KeyPair
	$req = New-Object Org.BouncyCastle.Pkcs.Pkcs10CertificationRequest(
		$signatureAlgorithm, $subjectDN, $keyPair.Public, $null, $keyPair.Private)
	$bytes = $req.GetEncoded()
	$path = Join-Path (Resolve-Path .) 'certreq.csr'
	[System.IO.File]::WriteAllBytes($path, $bytes)

This writes a `.DER`-formatted certificate request to the file `certreq.csr`.
If we inspect it by using OpenSSL, it looks pretty good:

	C:\OpenSSL-Win64\bin\openssl.exe req -in .\certreq.csr -noout -text -inform der

This gives us:
 
	Certificate Request:
	    Data:
	        Version: 0 (0x0)
	        Subject: CN=Whatever
	...etc.

Signing the request
-------------------

If you send that request to an Active Directory Certificate Authority for signing
(**and select the Web Server template**), it generates a server certificate.

And that appears to be pretty much that.

Wrap-Up
-------

I've added this as two new cmdlets, `New-CertificateRequest` and `Save-DerEncoded`,
to my [PSBouncyCastle](https://github.com/rlipscombe/PSBouncyCastle) module.