---
title: Bouncy Castle - Being a Certificate Authority
date: 2013-03-24T16:14:15Z
tags: bouncy-castle
---
Over the last few posts, we've seen how to create a self-signed server certificate in C#, by using the Bouncy Castle library. How do we create a CA certificate, and how do we issue certificates from that authority?

What's different about a CA certificate?
--

Two things:

* It claims to be a CA certificate.
* It's installed in the "Trusted Root Certificate Authorities" certificate store.

That's it. A CA certificate is just a self-signed certificate that's been installed in the correct store.

Claiming to be a CA Certificate
--

When we created our original self-signed certificate, [we added](https://blog.differentpla.net/b/2013/21/20/bouncy-castle---missing-certificate-attributes#basicconstraints) a "Basic Constraints" extension. We passed `false` as the `cA` parameter. For our certificate to assert that it's a CA certificate, we need to pass `true` instead:

    certificateGenerator.AddExtension(
	    X509Extensions.BasicConstraints.Id, true, new BasicConstraints(true));

And that really is all we need to do.

Issuing Certificates
--

There's nothing particularly complicated about issuing a certificate. Let's assume that we have a file, `CA.pfx`, which contains our CA certificate and private key.

We need to load the existing certificate:

	const string password = "password";
	var issuerCertificate = new X509Certificate2(issuerFileName, password);

We need to get the issuer name from that certificate:

	var issuerName = issuerCertificate.Subject;

We need to get the key-pair from the issuer certificate:

    var issuerKeyPair = DotNetUtilities.GetKeyPair(issuerCertificate.PrivateKey);

We need to get the serial number from the issuer certificate:

    var issuerSerialNumber = new BigInteger(issuerCertificate.GetSerialNumber());

And we're done.

Where's the source?
--

You can find the source code for this series of blog posts [on github](http://github.com/rlipscombe/bouncy-castle-csharp).

How do I use it?
--

To create a self-signed server certificate:

	CreateCertificate self CN=server server.pfx

To create a CA certificate:

	CreateCertificate ca CN=DemoCA CA.pfx

To issue a certificate using that CA:

	CreateCertificate issue CA.pfx CN=issued issued.pfx
