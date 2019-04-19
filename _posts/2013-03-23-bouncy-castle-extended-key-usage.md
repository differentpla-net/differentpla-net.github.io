---
title: Bouncy Castle - Extended Key Usage
date: 2013-03-23T13:13:35Z
tags: bouncy-castle
---
If you look at a certificate, you will see something like one of the following:

**This certificate is intended for the following purpose(s):**

* Protects e-mail messages
* Ensures the identity of a remote computer
* Ensures software came from software publisher
* Protects software from alteration after publication
* All issuance policies

There are two ways to specify these purposes: you can use the "key usage" extension, which is specified in [section 4.2.1.3](http://tools.ietf.org/html/rfc5280#section-4.2.1.3) of RFC 5820, or you can use the "extended key usage" extension, which is specified in [section 4.2.1.12](http://tools.ietf.org/html/rfc5280#section-4.2.1.12). The extended key usage extension is more flexible.

To specify that a certificate can be used for "server authentication", we can use the following code:

	// Add the "Extended Key Usage" attribute, specifying "server authentication".
	var usages = new[] { KeyPurposeID.IdKPServerAuth };
	certificateGenerator.AddExtension(
	    X509Extensions.ExtendedKeyUsage.Id,
		false,
		new ExtendedKeyUsage(usages));

In the next post, I'll discuss "subject alternative names", and then I'll show how to create CA certificates and how to use those to issue server and client certificates. Then I think that's about it for this series of posts.
