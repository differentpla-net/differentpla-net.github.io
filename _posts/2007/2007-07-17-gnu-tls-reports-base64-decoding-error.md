---
title: "GNU TLS reports \"Base64 Decoding Error\""
date: 2007-07-17T18:54:22.000Z
---
I recently had a problem getting STARTTLS working on exim4 on Debian 4.0 (Etch). It kept reporting:

<pre>TLS error on connection from _host_ (_ehlo_) [_ip_]
(cert/key setup: cert=/etc/ssl/certs/_whatever.crt_ key=/etc/ssl/private/_whatever.key_:
Base64 decoding error.</pre>

This was confirmed by running `gnutls-serv --debug 5 --x509certfile foo.crt --x509keyfile foo.key`, which reported the same error.

Turns out that what it actually means is not "Base64 decoding error". What it actually means is "You didn't remove the pass phrase from the key".

To do that:

<pre>$ cp foo.key foo.key.orig
$ openssl rsa -in foo.key.orig --out foo.key
</pre>
