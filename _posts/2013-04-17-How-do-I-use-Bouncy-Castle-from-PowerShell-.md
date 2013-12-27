---
layout: post
title: "How do I use Bouncy Castle from PowerShell?"
date: 2013-04-17T09:54:27.469Z
tags: powershell bouncy-castle certificates
alias: /post/UW5xwCp74I4PAAAB/how-do-i-use-bouncy-castle-from-powershell-
---

I recently wrote a [series of blog posts](http://blog.differentpla.net/b/2013/21/18/how-do-i-create-a-self-signed-certificate-using-bouncy-castle-)
on how to use the cryptography libraries from the [Legion of the BouncyCastle](http://www.bouncycastle.org/) in C#.

Here's where I show how to use the libraries from PowerShell.

I'll assume that you've read the other blog posts; this is going to be a breakneck race through getting that lot to work in PowerShell.

Generating Random Numbers
--

    $randomGenerator = New-Object Org.BouncyCastle.Crypto.Prng.CryptoApiRandomGenerator
    $random = New-Object Org.BouncyCastle.Security.SecureRandom($randomGenerator)

One problem with PowerShell is that it doesn't have a `using` declaration, and you have to fully-qualify type names.
So, in order to make life easier, I usually wrap this sort of thing in a function...

    function New-SecureRandom
	{
	    $randomGenerator = New-Object Org.BouncyCastle.Crypto.Prng.CryptoApiRandomGenerator
    	$random = New-Object Org.BouncyCastle.Security.SecureRandom($randomGenerator)
	}

The Certificate Generator
--

	function New-CertificateGenerator
	{
		$certificateGenerator = New-Object Org.BouncyCastle.X509.X509V3CertificateGenerator
		return $certificateGenerator
	}

Then we can use it as follows:

	$certificateGenerator = New-CertificateGenerator

Serial Number
--

	function New-SerialNumber(
		[Org.BouncyCastle.Security.SecureRandom] $random)
	{
		$serialNumber =
			[Org.BouncyCastle.Utilities.BigIntegers]::CreateRandomInRange(
				[Org.BouncyCastle.Math.BigInteger]::One,
				[Org.BouncyCastle.Math.BigInteger]::ValueOf([Int64]::MaxValue),
				$random)
	
		return $serialNumber
	}

	$random = New-SecureRandom
	$serialNumber = New-SerialNumber $random
	
	$certificateGenerator.SetSerialNumber($serialNumber)

Signature Algorithm
--

*Note:* You can get the list of available signature algorithms by:

	PS> $certificateGenerator.SignatureAlgNames
	SHA224WITHRSAANDMGF1
	GOST3411WITHGOST3410-94
	# ...
	SHA256WITHRSA
	# ...
	RIPEMD128WITHRSAENCRYPTION

As with the C# example, we'll use SHA1 with RSA...

    $signatureAlgorithm = "SHA256WithRSA"
    $certificateGenerator.SetSignatureAlgorithm($signatureAlgorithm)

Issuer and Subject Name
--

	$issuerDN = New-Object Org.BouncyCastle.Asn1.X509.X509Name($issuerName)
	$subjectDN = New-Object Org.BouncyCastle.Asn1.X509.X509Name($subjectName)
    $certificateGenerator.SetIssuerDN($issuerDN)
    $certificateGenerator.SetSubjectDN($subjectDN)

Valid For
--

	$notBefore = [DateTime]::UtcNow.Date
	$notAfter = $notBefore.AddYears(2)

	$certificateGenerator.SetNotBefore($notBefore)
	$certificateGenerator.SetNotAfter($notAfter)

Subject Public Key
--

	function New-KeyPair
	{
	param(
	    [Parameter(Mandatory = $true)]
	    [Org.BouncyCastle.Security.SecureRandom] $random,
	
	    [Parameter(Mandatory = $false)]
	    [int] $strength = 2048
	)
	
	    $keyGenerationParameters =
			New-Object Org.BouncyCastle.Crypto.KeyGenerationParameters($random, $strength)
	
	    $keyPairGenerator =
			New-Object Org.BouncyCastle.Crypto.Generators.RsaKeyPairGenerator

	    $keyPairGenerator.Init($keyGenerationParameters)
	    $keyPair = $keyPairGenerator.GenerateKeyPair()
	
	    return $keyPair
	}
	
	$subjectKeyPair = New-KeyPair $random
	$certificateGenerator.SetPublicKey($subjectKeyPair.Public)

Authority Key Identifier
--

See ["Missing Certificate Attributes"](http://blog.differentpla.net/b/2013/21/20/bouncy-castle---missing-certificate-attributes)
for discussion of how to do this in C#.

	function New-AuthorityKeyIdentifier
	{
	param(
	    [Parameter(Mandatory = $true)]
	    [string] $name,
	
	    [Parameter(Mandatory = $true)]
	    [Org.BouncyCastle.Crypto.Parameters.RsaKeyParameters] $publicKey,
	
	    [Parameter(Mandatory = $true)]
	    [Org.BouncyCastle.Math.BigInteger] $serialNumber
	)
	
	    $publicKeyInfo =
			[Org.BouncyCastle.X509.SubjectPublicKeyInfoFactory]::CreateSubjectPublicKeyInfo($publicKey)

	    $generalName = New-Object Org.BouncyCastle.Asn1.X509.GeneralName($name)
	    $generalNames = New-Object Org.BouncyCastle.Asn1.X509.GeneralNames($generalName)

	    $authorityKeyIdentifier =
	        New-Object Org.BouncyCastle.Asn1.X509.AuthorityKeyIdentifier(
	            $publicKeyInfo, $generalNames, $serialNumber)

		return $authorityKeyIdentifier
	}

For the next bit, we'll go a bit "fluent". I'll discuss the pros and cons of this in a later blog post:

	function Add-AuthorityKeyIdentifier
	{
	[CmdletBinding()]
	param(
	    [Parameter(Mandatory = $true, ValueFromPipeline = $true)]
	    [Org.BouncyCastle.X509.X509V3CertificateGenerator] $certificateGenerator,
	
	    [Parameter(Position = 0, Mandatory = $true, ValueFromPipeline = $false)]
	    [Org.BouncyCastle.Asn1.X509.AuthorityKeyIdentifier] $authorityKeyIdentifier
	)
	
	    $certificateGenerator.AddExtension(
	        [Org.BouncyCastle.Asn1.X509.X509Extensions]::AuthorityKeyIdentifier.Id,
	        $false,
	        $authorityKeyIdentifier)
	
	    return $certificateGenerator
	}
	
This allows us to use the certificate generator as follows:

	# Fluent; less repetition.
	$certificateGenerator |
		Add-AuthorityKeyIdentifier $authorityKeyIdentifier |
		Add-SubjectKeyIdentifier $subjectKeyIdentifier

...which is a bit more readable (in my opinion) than the following:

	# Non-fluent; more verbose.
	Add-AuthorityKeyIdentifier $certificateGenerator $authorityKeyIdentifier
	Add-SubjectKeyIdentifier $certificateGenerator $subjectKeyIdentifier

Subject Key Identifier
--

This bit's simpler than the authority key identifer...

	function New-SubjectKeyIdentifier
	{
	param(
	    [Parameter(Mandatory = $true)]
	    [Org.BouncyCastle.Crypto.Parameters.RsaKeyParameters] $publicKey
	)
	
	    $publicKeyInfo =
	        [Org.BouncyCastle.X509.SubjectPublicKeyInfoFactory]::CreateSubjectPublicKeyInfo($publicKey)
	
	    $subjectKeyIdentifier =
	        New-Object Org.BouncyCastle.Asn1.X509.SubjectKeyIdentifier($publicKeyInfo)
	
	    return $subjectKeyIdentifier
	}

This bit's more-or-less the same:

	function Add-SubjectKeyIdentifier
	{
	[CmdletBinding()]
	param(
	    [Parameter(Mandatory = $true, ValueFromPipeline = $true)]
	    [Org.BouncyCastle.X509.X509V3CertificateGenerator] $certificateGenerator,
	
	    [Parameter(Position = 0, Mandatory = $true, ValueFromPipeline = $false)]
	    [Org.BouncyCastle.Asn1.X509.SubjectKeyIdentifier] $subjectKeyIdentifier
	)
	
	    $certificateGenerator.AddExtension(
	        [Org.BouncyCastle.Asn1.X509.X509Extensions]::SubjectKeyIdentifier.Id,
	        $false,
	        $subjectKeyIdentifier)
	
	    return $certificateGenerator
	}

Basic Constraints
--

	function Add-BasicConstraints
	{
	[CmdletBinding()]
	param(
	    [Parameter(Mandatory = $true, ValueFromPipeline = $true)]
	    [Org.BouncyCastle.X509.X509V3CertificateGenerator] $certificateGenerator,
	
	    [Parameter(Position = 0, Mandatory = $true, ValueFromPipeline = $false)]
	    [bool] $isCertificateAuthority
	)
	
	    $basicConstraints =
	        New-Object Org.BouncyCastle.Asn1.X509.BasicConstraints($isCertificateAuthority)
	    $certificateGenerator.AddExtension(
	        [Org.BouncyCastle.Asn1.X509.X509Extensions]::BasicConstraints.Id,
	        $true,
	        $basicConstraints)
	
	    return $certificateGenerator
	}

Generating the Certificate
--

If it's self-signed, we can do this:

	$issuerKeyPair = $subjectKeyPair
	$certificate = $certificateGenerator.Generate($issuerKeyPair.Private, $random)

Converting to a .NET Certificate
--

See the discussion of
[how to do this in C#](http://blog.differentpla.net/b/2013/21/18/how-do-i-convert-a-bouncy-castle-certificate-to-a-net-certificate-) first.
This is that, directly translated into PowerShell.

	function ConvertFrom-BouncyCastleCertificate
	{
	param(
	    [Parameter(Mandatory = $true)]
	    [Org.BouncyCastle.X509.X509Certificate] $certificate,
	
	    [Parameter(Mandatory = $true)]
	    [Org.BouncyCastle.Crypto.AsymmetricCipherKeyPair] $subjectKeyPair,
	
	    [Parameter(Mandatory = $true)]
	    [string] $friendlyName
	)

	    $store = New-Object Org.BouncyCastle.Pkcs.Pkcs12Store
	
	    $certificateEntry = New-Object Org.BouncyCastle.Pkcs.X509CertificateEntry($certificate)
	    $store.SetCertificateEntry($friendlyName, $certificateEntry)
	
	    $keyEntry = New-Object Org.BouncyCastle.Pkcs.AsymmetricKeyEntry($subjectKeyPair.Private)
	    $store.SetKeyEntry($friendlyName, $keyEntry, @($certificateEntry))

	    # The password is re-used immediately, so it doesn't matter what it is.
	    $password = 'password'
	    $stream = New-Object System.IO.MemoryStream
	    $store.Save($stream, $password, $random)
	
	    $keyStorageFlags = 'PersistKeySet, Exportable'
	    $result =
			New-Object System.Security.Cryptography.X509Certificates.X509Certificate2(
				$stream.ToArray(), $password, $keyStorageFlags)
	
	    $stream.Dispose()
	
	    return $result
	}

Note that we don't bother writing the certificate to a `.PFX` file, since it's perfectly usable as it is.

Putting it all together
--

The source code's in my [`PSBouncyCastle` GitHub repository](https://github.com/rlipscombe/PSBouncyCastle).
