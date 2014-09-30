---
title: PowerShell, Bouncy Castle and Extended Key Usage
date: 2013-04-17T12:46:44Z
---
*Note: This is a bit long, because I want to take a moment to show some of the problems you might have using PowerShell to call .NET code that's written in a certain style.*

*If you can't be bothered, skip to the end, or the source code's in [the repository](https://github.com/rlipscombe/PSBouncyCastle).*

Overview
--

In [the C# code](http://blog.differentpla.net/b/2013/21/23/bouncy-castle---extended-key-usage), we had something like the following:

    // Add the "Extended Key Usage" attribute, specifying "server authentication".
	var usages = new[] { KeyPurposeID.IdKPServerAuth };
	var extendedKeyUsage = new ExtendedKeyUsage(usages);
	certificateGenerator.AddExtension(
	    X509Extensions.ExtendedKeyUsage.Id,
	    false,
	    extendedKeyUsage);

This translates directly to PowerShell:

	$usages = @( [Org.BouncyCastle.Asn1.X509.KeyPurposeID]::IdKPServerAuth )
	$extendedKeyUsage =
		New-Object Org.BouncyCastle.Asn1.X509.ExtendedKeyUsage($usages)

...but we'd kinda like to allow the following:

	$certificateGenerator | Add-ExtendedKeyUsage -Oid '1.3.6.1.5.5.7.3.1'

...or maybe...

	$certificateGenerator | Add-ExtendedKeyUsage -ServerAuth

KeyPurposeID
--

PowerShell has a really cool feature where it'll [coerce a string into an `enum` value](http://blog.differentpla.net/b/2013/31/17/powershell-and-enums), but that's no good to us here:

	PS> [Org.BouncyCastle.Asn1.X509.KeyPurposeID] $usage = 'IdKPServerAuth'
	Cannot convert the "IdKPServerAuth" value of type "System.String"
		to type "Org.BouncyCastle.Asn1.X509.KeyPurposeID".

...because `KeyPurposeID` isn't a C#-style enum, it's a Java-style enum class, where it contains static members with the appropriate values:

	PS> [Org.BouncyCastle.Asn1.X509.KeyPurposeID].GetMembers() |
			where { $_.MemberType -eq 'Field' } | % { $_.Name }
	AnyExtendedKeyUsage
	IdKPServerAuth
	IdKPClientAuth
	...

Converting a string OID (doesn't work)
--

So, how do we get from a string OID ('1.3.6.1.5.5.7.3.1') to a `KeyPurposeID`?

`KeyPurposeID` has no constructors:

	PS> [Org.BouncyCastle.Asn1.X509.KeyPurposeID].GetConstructors().Count
	0

`KeyPurposeID` is derived from `DerObjectIdentifier`:

	PS> [Org.BouncyCastle.Asn1.X509.KeyPurposeID].BaseType.FullName
	Org.BouncyCastle.Asn1.DerObjectIdentifier

...which does have a constructor:

	PS> [Org.BouncyCastle.Asn1.DerObjectIdentifier].GetConstructors() | % { $_.ToString() }
	Void .ctor(System.String)

Let's try that:

	PS> $oid = '1.3.6.1.5.5.7.3.1'
	PS> $usage = New-Object Org.BouncyCastle.Asn1.X509.KeyPurposeID($oid)

	New-Object : Constructor not found. Cannot find an appropriate constructor for type
	Org.BouncyCastle.Asn1.X509.KeyPurposeID.

Nope.

Converting a string OID
--

OK. Let's try something different:

	PS> [Org.BouncyCastle.Asn1.X509.ExtendedKeyUsage].GetConstructors() | % { $_.ToString() }
	Void .ctor(Org.BouncyCastle.Asn1.X509.KeyPurposeID[])
	Void .ctor(System.Collections.ArrayList)
	Void .ctor(System.Collections.IEnumerable)

Ah. No generics. So what is it expecting in that `IEnumerable`? Time to take a look at the [source code](http://www.bouncycastle.org/viewcvs/viewcvs.cgi/csharp/crypto/src/asn1/x509/ExtendedKeyUsage.cs?revision=1.22&view=markup)...

	public ExtendedKeyUsage(IEnumerable usages)
	{
		Asn1EncodableVector v = new Asn1EncodableVector();
		foreach (Asn1Object o in usages)
		{
			// ...

OK, so it wants `Asn1Object` instances. Well, we're in luck because `DerObjectIdentifier` that we were looking at earlier is derived from `Asn1Object`. This means that we can do something like the following:

	$oid = '1.3.6.1.5.5.7.3.1'
	$usage = New-Object Org.BouncyCastle.Asn1.DerObjectIdentifier($oid)
	$usages =  @( $usages )
	$extendedKeyUsage =
		New-Object Org.BouncyCastle.Asn1.X509.ExtendedKeyUsage($usages)

But, in our function, which starts like this...

	function Add-ExtendedKeyUsage
	{
	[CmdletBinding()]
	param(
	    [Parameter(Mandatory = $true, ValueFromPipeline = $true)]
	    [Org.BouncyCastle.X509.X509V3CertificateGenerator] $certificateGenerator,
	
	    [Parameter(Position = 0, Mandatory = $false, ValueFromPipeline = $false)]
	    [string[]] $Oid = $null
	)
	
...we want something like the following:

    $usages = $Oid | % { New-Object Org.BouncyCastle.Asn1.DerObjectIdentifier($_) }
    $extendedKeyUsage = New-Object Org.BouncyCastle.Asn1.X509.ExtendedKeyUsage($usages)

We convert our string array into an array of `DerObjectIdentifier`. Seems fairly simple, right? Doesn't work:

	New-Object : Cannot find an overload for "ExtendedKeyUsage" and the argument count: "1".

What's happening here is that we have a `DerObjectIdentifier[]`, and PowerShell can't figure out which constructor to use. We need to either construct an `ArrayList`, or coerce this into an `IEnumerable`. We'll try the `IEnumerable` first:

	PS> $usages = $Oid | % { New-Object Org.BouncyCastle.Asn1.DerObjectIdentifier($_) }
	PS> $extendedKeyUsage =
			New-Object Org.BouncyCastle.Asn1.X509.ExtendedKeyUsage(
				[System.Collections.IEnumerable] $usages)
	Cannot convert the "1.3.6.1.5.5.7.3.1" value of type "Org.BouncyCastle.Asn1.DerObjectIdentifier"
		to type "System.Collections.IEnumerable".

What's happening here is that PowerShell likes to unwrap single-item arrays:

	PS> $usages.GetType().IsArray
	False

This answer is to [add a comma](http://stackoverflow.com/q/9130045/8446). Yes, really. But, we also need to use an array of the correct type:

    [Org.BouncyCastle.Asn1.Asn1Object[]] $usages = @()
    $Oid | % { $usages += New-Object Org.BouncyCastle.Asn1.DerObjectIdentifier($_) }
    $extendedKeyUsage = New-Object Org.BouncyCastle.Asn1.X509.ExtendedKeyUsage(,$usages)

