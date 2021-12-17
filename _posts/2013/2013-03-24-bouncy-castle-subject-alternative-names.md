---
title: Bouncy Castle - Subject Alternative Names
date: 2013-03-24T14:20:58Z
redirect_from: /b/2013/21/24/bouncy-castle---subject-alternative-names
tags: bouncy-castle
layout: series
series: bouncy-castle
---
When you connect to a server using HTTPS, the server provides a certificate that identifies it. Your browser will then typically check that the name in the server certificate matches the address that you've gone to. In this way, you can be sure that you're connecting to the correct server.

Also, when you connect to a web server, you can use the `Host:` header to tell it which site you want to visit. In this way, multiple web sites can be hosted on a single IP address.

However, there's a bit of a chicken-and-egg situation going on here: the server can't read the `Host:` header until the SSL (actually TLS) session is established, it can't establish the secure session without providing a server certificate, and it doesn't know which certificate to provide until it's seen the `Host:` header.

A bit of a quandary that one. There are a few ways around it:

## Use Server Name Indication (SNI)

Server Name Indication is an extension to the TLS protocol that allows the client to indicate which server it's trying to reach, in a similar way to the HTTP `Host:` header.

For this to work, your client needs to be using Internet Explorer 7 or later. I'll assume that users who aren't using IE are clued up enough to be using the latest and greatest Firefox, Chrome, Safari or whatever.

However, if you're stuck with IIS on Windows, you need to be using IIS 8, which means Windows Server 2012.

## Use a wildcard certificate

This one's easy: just issue a certificate for (e.g.) `CN=*.mydomain.com`. This certificate can then be used for any server in that domain (provided the certificate's trusted, of course).

## Use a Subject Alternative Name extension

In other places, you might see this referred to as a Universal Communication (UC) certificate. In short, it allows a certificate to have more than one subject name.

To add this extension, you need to use the following code:

    var subjectAlternativeNames = new Asn1Encodable[]
	    {
	        new GeneralName(GeneralName.DnsName, "server"),
	        new GeneralName(GeneralName.DnsName, "server.mydomain.com")
	    };
	var subjectAlternativeNamesExtension = new DerSequence(subjectAlternativeNames);
	certificateGenerator.AddExtension(
	    X509Extensions.SubjectAlternativeName.Id, false, subjectAlternativeNamesExtension);

Note that for this to work properly, you need to specify the value from the "Subject DN" property that you set earlier.
