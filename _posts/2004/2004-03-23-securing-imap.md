---
title: "Securing IMAP"
date: 2004-03-23T11:39:00.000Z
redirect_from: /node/view/190
tags: qmail
layout: series
series: qmail
---
BincIMAP supports IMAPS for communication. It can either do this if you compile in SSL support, or if you use an SSL tunnel, such as [stunnel](http://www.stunnel.org/) or [ucspi-ssl](http://www.superscript.com/ucspi-ssl/intro.html).

I'm going to use the built-in SSL support. This got included when I [installed BincIMAP earlier]({% post_url 2004/2004-03-11-installing-bincimap %}). To make it work, you need to create a `/etc/opt/bincimap/bincimap.pem` file containing your server key and server certificate. Since you'll have already generated a key and certificate for [using with SquirrelMail]({% post_url 2004/2004-03-18-securing-squirrelmail-using-https %}), you can just use this:

<pre># cat /etc/apache/ssl.key/server.key /etc/apache/ssl.crt > /etc/opt/bincimap/bincimap.pem
# chmod 400 /etc/opt/bincimap/bincimap.pem</pre>

Now you can configure your email client to require secure IMAP connections. In Outlook XP, you can just check the box marked "This server requires an SSL-secured connection (SSL):

![[broken image]](/images/a470728f1b7c8c6154cbf7bc74425fdd-191.png)

And it'll just work.

## SSL Certificates: Common Names

When we created the certificate for `flimsy`, the common name used was `flimsy.home.differentpla.net`. Connections to anything else will result in a "The name on the security certificate is invalid or does not match the name of the site" error, or words to that effect.

This could cause some problems: webmail users will typically be connecting to `www.somewhere.com`. This looks a bit daft for IMAP users. They'd rather connect to `imap.somewhere.com` or `mail.somewhere.com`.

If you plunk down more cash, you can buy what's called a [wildcard certificate](http://www.sslreview.com/content/wildcard-ssl.html).

It doesn't appear that you can generate self-signed wildcard certificates, so we'll just have to create two certificates: one for HTTPS and one for IMAPS:

```
$ openssl genrsa -out www.differentpla.test.key 1024
$ openssl req -new -key www.differentpla.test.key -out www.differentpla.test.csr
$ openssl genrsa -out imap.differentpla.test.key 1024
$ openssl req -new -key imap.differentpla.test.key -out imap.differentpla.test.csr
```

Then send those off to be signed, or sign them with your own CA.

When they come back, install them as you did before, but this time, you'll keep the HTTPS and IMAPS certificates separate.

**Important:** Remember that they're not protected with a passphrase, so make sure that they're readable only by `root`.

Now, users should be able to connect to `imap.differentpla.test` using IMAPS, be presented with no warning messages and be able to bask in the warm feeling of secure IMAP.
