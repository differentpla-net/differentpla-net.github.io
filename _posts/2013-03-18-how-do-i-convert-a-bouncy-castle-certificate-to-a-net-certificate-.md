---
title: How do I convert a Bouncy Castle Certificate to a .NET Certificate?
date: 2013-03-18T19:57:54Z
tags: bouncy-castle
---
In the last installment, we created a *Bouncy Castle* certificate. How do we now create a .NET `X509Certificate2` object?

The only reliable way I've found to do this is to create a `PKCS12` file. On Windows, these are more commonly known as `.PFX` files. This is based on a solution I found [here](http://web.archive.org/web/20100504192226/http://www.fkollmann.de/v2/post/Creating-certificates-using-BouncyCastle.aspx).

    var store = new Pkcs12Store();

Bouncy Castle uses an "alias" to associate the public key and private key within the container. Windows refers to this as the certificate "friendly name". For this example, we'll use the subject name.

	string friendlyName = certificate.SubjectDN.ToString();

We then need to add the certificate to the store.

	var certificateEntry = new X509CertificateEntry(certificate);
	store.SetCertificateEntry(friendlyName, certificateEntry);

Then we add the private key. It needs to have the same alias, and needs to be associated with the certificate entry that we just added.

	store.SetKeyEntry(friendlyName, new AsymmetricKeyEntry(subjectKeyPair.Private), new[] { certificateEntry });

Now we've got a `Pkcs12Store`, we can copy it to a stream. For this part, we need to specify a password:

	const string password = "password";

	var stream = new MemoryStream();
	store.Save(stream, password.ToCharArray(), random);

With that stream, we have two options. We can write a `.PFX` file:

	File.WriteAllBytes("foo.pfx", stream.ToArray());

Note that to import this file, you'll need to specify the password.

Alternatively, at this point, you can convert it to a .NET `X509Certificate2` object.

	var convertedCertificate =
	    new X509Certificate2(
	        stream.ToArray(), password,
	        X509KeyStorageFlags.PersistKeySet | X509KeyStorageFlags.Exportable);

I'll write something about what the flags mean later...

And you can import that directly into the Windows Certificate store, should you want to.
