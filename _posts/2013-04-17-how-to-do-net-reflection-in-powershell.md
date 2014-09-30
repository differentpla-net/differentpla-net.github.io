---
title: How to do .NET Reflection in PowerShell
date: 2013-04-17T11:29:29Z
---
This is all just .NET reflection code, converted from C# to PowerShell, but it can be useful...

Finding out which assemblies are loaded
--

    PS> [AppDomain]::CurrentDomain.GetAssemblies()

	GAC    Version        Location
	---    -------        --------
	True   v4.0.30319     C:\Windows\Microsoft.NET\Framework64\v4.0.30319\mscorlib.dll
	True   v4.0.30319     C:\Windows\Microsoft.Net\assembly\GAC_MSIL\Microsoft.PowerShell.ConsoleHost\v...
	# etc.

Finding out which types are available in a given assembly
--

	PS> $assembly = [AppDomain]::CurrentDomain.GetAssemblies() | where { $_.Location -like '*System.Numerics*' }
	PS> $assembly.GetExportedTypes()

	IsPublic IsSerial Name                                     BaseType
	-------- -------- ----                                     --------
	True     True     BigInteger                               System.ValueType
	True     True     Complex                                  System.ValueType

	PS> $assembly.GetExportedTypes() | % { $_.FullName }
	System.Numerics.BigInteger
	System.Numerics.Complex

Get the members of a particular type
--

	PS> [System.Numerics.Complex] | Get-Member
	# ...list of members...

Get a list of constructors for a particular type
--

	PS> [Org.BouncyCastle.Asn1.X509.ExtendedKeyUsage].GetConstructors() |
			foreach { $_.ToString() }
	
	Void .ctor(Org.BouncyCastle.Asn1.X509.KeyPurposeID[])
	Void .ctor(System.Collections.ArrayList)
	Void .ctor(System.Collections.IEnumerable)

