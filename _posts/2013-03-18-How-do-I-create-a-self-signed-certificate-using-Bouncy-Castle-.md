---
layout: post
title: "How do I create a self-signed certificate using Bouncy Castle?"
date: 2013-03-18T18:57:34.994Z
tags: certificates bouncy-castle
alias: /post/UUdjnnuYkMwsAAAB/how-do-i-create-a-self-signed-certificate-using-bouncy-castle-
---

Occasionally, you might find that you need to create a self-signed server certificate. If you're on Windows, your options are basically:

 * Use `makecert` from the Windows SDK.
 * Install OpenSSL.
 * Use the [Crypto API](http://msdn.microsoft.com/en-us/library/windows/desktop/aa380255.aspx). This is hard enough to use from C++. If you're on C#, [good luck with that](http://blogs.msdn.com/b/dcook/archive/2008/11/25/creating-a-self-signed-certificate-in-c.aspx).
 * Head over to the [Legion of the Bouncy Castle](http://www.bouncycastle.org/csharp/), and use their excellent API, which covers all manner of cryptography shenanigans.

Using Bouncy Castle
--

In this example, I'll show you how to create a self-signed server certificate using C# and the Bouncy Castle .NET API.

In order to follow along, create a new project and then use NuGet to add the "BouncyCastle" package.

The basic idea is this:

1. Create a `CertificateGenerator`.
2. Set a bunch of properties.
3. Ask it to generate the certificate.

What's in a certificate?
--

At its most basic a certificate contains the following:

 * A subject name. This is the name of the person or server who will use the certificate.
 * A public key.

...and the certificate is signed using the issuer's private key.

At some point in the future, I'll write something more about how public and private keys are used with certificates, and SSL in particular. For now, just remember that asymmetric encryption requires both a public key and a private key. The server keeps the private key, and the certificate contains the public key.

Also present in a certificate are usually the following fields:

 * A serial number, which is used for revocation.
 * A thumbprint; this is the bit signed by the issuer's private key.
 * The issuer's name.
 * A date range for which the certificate is valid.

Note that, for Bouncy Castle, if you don't fill in the following fields, you'll get an `InvalidOperationException` containing "not all mandatory fields set in V3 TBScertificate generator":

 * Serial number
 * Signature algorithm
 * Issuer Name.
 * Subject Name -- or a Subject Alternative Name (SAN).
 * Date range (not before, not after).
 * Subject Public Key.

In the case of self-signed certificates, the subject and the issuer are one and the same, which simplifies the code a little.

Generating Random Numbers
--

We're going to need some random numbers later, so create a RNG first. Since we're on Windows, we'll use the CryptoAPI one (on the assumption that it might have access to better sources of entropy than the built-in Bouncy Castle ones):

    var randomGenerator = new CryptoApiRandomGenerator();
	var random = new SecureRandom(randomGenerator);

The Certificate Generator
--

Then we need a certificate generator:

	var certificateGenerator = new X509V3CertificateGenerator();

Serial Number
--

The certificate needs a serial number. This is used for revocation, and usually should be an incrementing index (which makes it easier to revoke a range of certificates).

Since we don't have anywhere to store the incrementing index, we can just use a random number.

	var serialNumber =
	    BigIntegers.CreateRandomInRange(
	        BigInteger.One, BigInteger.ValueOf(Int64.MaxValue), random);
	certificateGenerator.SetSerialNumber(serialNumber);

Signature Algorithm
--

When we say that the certificate is signed using the issuer's private key, what we actually mean is that the certificate is hashed to create a smaller hash value, and then that hash value is encrypted using the issuer's private key.

For this, we need to specify a *signature algorithm*. Algorithms that you might have heard of are are MD5 (totally broken, you should never use it) and SHA-1 (probably broken; no longer recommended). For certificates, the current recommendation seems to be SHA-256 or SHA-512.

For this example, we'll use SHA-256.

	const string signatureAlgorithm = "SHA256WithRSA";
	certificateGenerator.SetSignatureAlgorithm(signatureAlgorithm);

Note that you'll still see "SHA1" for the "Thumbprint Algorithm" property. This is [expected](http://social.technet.microsoft.com/Forums/en-US/winserversecurity/thread/9543cd5b-c3b3-4d13-a9c4-46b97f2c6c18/): the thumbprint is not the same as the signature.

Issuer and Subject Name
--

Then we have to specify the issuer name and subject name. Since this is a self-signed certificate, these are the same.

	var subjectDN = new X509Name(subjectName);
	var issuerDN = subjectDN;
	certificateGenerator.SetIssuerDN(issuerDN);
    certificateGenerator.SetSubjectDN(subjectDN);

Note that Bouncy Castle allows you to omit the subject name, provided you specify a Subject Alternative Name (SAN).

Valid For
--

We need to specify a date range for which this certificate is valid:

	var notBefore = DateTime.UtcNow.Date;
	var notAfter = notBefore.AddYears(2);
	
	certificateGenerator.SetNotBefore(notBefore);
	certificateGenerator.SetNotAfter(notAfter);

I can't remember whether the date and time are in UTC or local time, so I've opted to start the certificate from this morning, UTC. Since I'm in London, that'll work for me.

Certificates also have an expiry date/time. This controls how long the certificate is valid for.

Short-lived certificates are useful, because even if someone steals it, it's only valid for a short length of time. This means that the opportunity for mischief is limited (a bit, at least). More constructively, we don't need to maintain revocation information for certificates that have expired. However, the certificate will need replacing when it expires, so short lifetimes require more administration overhead.

In general, Root Certificate Authority certificates are relatively long-lived (20 years or so), intermediate CA certificates are valid for between 5 and 10 years, and end certificates usually last for anywhere between 1 and 3 years.

Subject Public Key
--

Finally, we need to generate the important bit: the subject's key pair. The public key goes into the certificate, and the private key is kept, well, private. I'll talk about how later.

	const int strength = 2048;
	var keyGenerationParameters = new KeyGenerationParameters(random, strength);
	
	var keyPairGenerator = new RsaKeyPairGenerator();
	keyPairGenerator.Init(keyGenerationParameters);
	var subjectKeyPair = keyPairGenerator.GenerateKeyPair();

	certificateGenerator.SetPublicKey(subjectKeyPair.Public);
	
In this example, `strength` is the key length, in bits. For RSA, 2048-bits should be considered the minimum acceptable these days.

Generating the Certificate
--

To generate the certificate, we need to provide the issuer's private key. Because this is a self-signed certificate, this is the same as the subject private key.

	var issuerKeyPair = subjectKeyPair;
    var certificate = certificateGenerator.Generate(issuerKeyPair.Private, random);

What's Next?
--

Well, we've generated an X509v3 certificate using the Bouncy Castle libraries. Unfortunately, we're still a short way from actually using it. I'll show how to export the certificate to a `.CER` file (and the private key to a `.PFX` file) in the next installment...
