---
title: "Using SMTP-TLS with qmail"
date: 2004-03-24T14:12:00.000Z
redirect_from: /node/view/196
---
Lucky part 13 of [Installing qmail and vpopmail](/node/view/165). This one talks about SMTP-over-TLS.

If you install [this patch](http://shupp.org/patches/netqmail-1.05-tls-smtpauth-20040207.patch) (mentioned [here](/node/view/167)), then you'll get SMTP-over-TLS in qmail.

It's implemented using the STARTTLS command, and goes over the standard SMTP port.

To get it to work, you'll need a server certificate, in PEM format. The simplest way to get hold of one is to copy the one you used when [securing IMAP](/node/view/190):

<pre># cp /etc/opt/bincimap/bincimap.pem /var/qmail/control/servercert.pem
# chmod 400 /var/qmail/control/servercert.pem</pre>

You'll also need to make it readable by the user account under which qmail-smtpd is running. For a normal installation, this is `qmaild.qmail`. For my vpopmail installation, it's `vpopmail.vchkpw`:

<pre># chown vpopmail.vchkpw /var/qmail/control/servercert.pem</pre>

If it's not working, you can debug it by telnetting to port 25 on your host and typing in the `STARTTLS` command. The output is usually pretty descriptive. For example:

<pre>220 peculiar.differentpla.net ESMTP
HELO
250 peculiar.differentpla.net
STARTTLS
454 TLS missing certificate: error:0200100D:system library:fopen:Permission denied (#4.3.0)</pre>

If it's working, you should see the following:

<pre>220 peculiar.differentpla.net ESMTP
HELO
250 peculiar.differentpla.net
STARTTLS
220 ready for tls</pre>

For more information, check out [http://iain.cx/ssl/?qmailtls](http://iain.cx/ssl/?qmailtls).

Next: [Installing qmailAdmin](/node/view/198).
