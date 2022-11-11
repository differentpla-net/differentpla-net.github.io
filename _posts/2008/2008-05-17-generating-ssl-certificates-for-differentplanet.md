---
title: "Generating SSL certificates for differentpla.net"
date: 2008-05-17T06:29:50.000Z
tags: openssl certificates
---
Because I keep forgetting how to.

## Preamble

<pre>$ **mkdir SSL**
$ **cd SSL**
$ **cp /usr/lib/ssl/misc/CA.pl .**</pre>

## Setting up a certificate authority

<pre>$ **./CA.pl -newca**
CA certificate filename (or enter to create)**Press Enter**

Making CA certificate ...
Generating a 1024 bit RSA private key
.......................++++++
.....................................................++++++
writing new private key to './demoCA/private/cakey.pem'
Enter PEM pass phrase:**Enter a passphrase to use for the CA**
Verifying - Enter PEM pass phrase:**Enter the same passphrase**
-----
You are about to be asked to enter information that will be incorporated
into your certificate request.
What you are about to enter is what is called a Distinguished Name or a DN.
There are quite a few fields but you can leave some blank
For some fields there will be a default value,
If you enter '.', the field will be left blank.
-----
Country Name (2 letter code) [AU]:**UK**
State or Province Name (full name) [Some-State]:**England**
Locality Name (eg, city) []:**London**
Organization Name (eg, company) [Internet Widgits Pty Ltd]:**differentpla.net**
Organizational Unit Name (eg, section) []:**Certificate Authority**
Common Name (eg, YOUR name) []:**ca.differentpla.net**
Email Address []:**Enter a valid email address**

Please enter the following 'extra' attributes
to be sent with your certificate request
A challenge password []:**Press Enter**
An optional company name []:**Press Enter**
Using configuration from /usr/lib/ssl/openssl.cnf
Enter pass phrase for ./demoCA/private/cakey.pem:**Enter the passphrase from above**
Check that the request matches the signature
Signature ok
Certificate Details:</pre>

(snip)

<pre>Certificate is to be certified until May 17 06:21:04 2011 GMT (1095 days)

Write out database with 1 new entries
Data Base Updated</pre>

## Stashing the CA certificate

<pre># cp /path/to/SSL/demoCA/cacert.pem /etc/ssl/certs/ca-differentpla-net.cer</pre>

## Generating and signing a certificate

<pre>$ **./CA.pl -newreq**
Generating a 1024 bit RSA private key
...............++++++
..........++++++
writing new private key to 'newkey.pem'
Enter PEM pass phrase:**Enter a passphrase**
Verifying - Enter PEM pass phrase:**Enter the same passphrase**
-----
You are about to be asked to enter information that will be incorporated
into your certificate request.
What you are about to enter is what is called a Distinguished Name or a DN.
There are quite a few fields but you can leave some blank
For some fields there will be a default value,
If you enter '.', the field will be left blank.
-----
Country Name (2 letter code) [AU]:**UK**
State or Province Name (full name) [Some-State]:**England**
Locality Name (eg, city) []:**London**
Organization Name (eg, company) [Internet Widgits Pty Ltd]:**differentpla.net**
Organizational Unit Name (eg, section) []:**Press Enter**
Common Name (eg, YOUR name) []:**smtp.differentpla.net**
Email Address []:**Enter a valid email address**

Please enter the following 'extra' attributes
to be sent with your certificate request
A challenge password []:**Press Enter**
An optional company name []:**Press Enter**
Request is in newreq.pem, private key is in newkey.pem
</pre>

<pre>$ **./CA.pl -sign**
Using configuration from /usr/lib/ssl/openssl.cnf
Enter pass phrase for ./demoCA/private/cakey.pem:**Enter the CA passphrase**
Check that the request matches the signature
Signature ok
Certificate Details:</pre>

(snip)

<pre>Certificate is to be certified until May 17 06:28:15 2009 GMT (365 days)
Sign the certificate? [y/n]:**y**

1 out of 1 certificate requests certified, commit? [y/n]**y**
Write out database with 1 new entries
Data Base Updated
Signed certificate is in newcert.pem</pre>

## Stripping the passphrase

<pre>$ **cp newkey.pem newkey.pem.org**
$ **openssl rsa -in newkey.pem.org -out newkey.pem**
Enter pass phrase for newkey.pem.org:**Enter the passphrase that you provided when generating the key**
writing RSA key</pre>

## Don't need these files any more

<pre>$ rm newreq.pem newkey.pem.org</pre>

## Storing the certificate and key files

<pre># cp /path/to/SSL/newcert.pem /etc/ssl/certs/smtp-differentpla-net.cer
# chmod a+r /etc/ssl/certs/smtp-differentpla-net.cer
# cp /path/to/SSL/newkey.pem /etc/ssl/private/smtp-differentpla-net.key
# chmod 400 /etc/ssl/private/smtp-differentpla-net.key</pre>

## Using that certificate for qmail

qmail needs a /var/qmail/control/servercert.pem file containing the key (no passphrase) followed by the certificate.

<pre># cat /etc/ssl/private/smtp-differentpla-net.key /etc/ssl/certs/smtp-differentpla-net.cer > /var/qmail/control/servercert.pem
# chmod 400 /var/qmail/control/servercert.pem
# chown vpopmail.vchkpw /var/qmail/control/servercert.pem</pre>

## Using a certificate for BincIMAP

<pre># cat /etc/ssl/private/imap-differentpla-net.key /etc/ssl/certs/imap-differentpla-net.cer > /usr/local/etc/bincimap.pem
# chown root.staff /usr/local/etc/bincimap.pem
# chmod 400 /usr/local/etc/bincimap.pem</pre>

## Making the certificates available to anyone that wants them

<pre># mkdir /path/to/www/certs
# cp /etc/ssl/certs/*.cer /path/to/www/certs
# chmod a+r /path/to/www/certs/*</pre>
