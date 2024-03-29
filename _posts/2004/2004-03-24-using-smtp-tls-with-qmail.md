---
title: "Using SMTP-TLS with qmail"
date: 2004-03-24T14:12:00.000Z
redirect_from: /node/view/196
tags: qmail
layout: series
series: qmail
---
If you install [this patch](http://shupp.org/patches/netqmail-1.05-tls-smtpauth-20040207.patch) (mentioned [here]({% post_url 2004/2004-03-11-installing-smtp-auth-with-qmail %})), then you'll get SMTP-over-TLS in qmail.

It's implemented using the STARTTLS command, and goes over the standard SMTP port.

To get it to work, you'll need a server certificate, in PEM format. The simplest way to get hold of one is to copy the one you used when [securing IMAP]({% post_url 2004/2004-03-23-securing-imap %}):

```
# cp /etc/opt/bincimap/bincimap.pem /var/qmail/control/servercert.pem
# chmod 400 /var/qmail/control/servercert.pem
```

You'll also need to make it readable by the user account under which qmail-smtpd is running. For a normal installation, this is `qmaild.qmail`. For my vpopmail installation, it's `vpopmail.vchkpw`:

```
# chown vpopmail.vchkpw /var/qmail/control/servercert.pem
```

If it's not working, you can debug it by telnetting to port 25 on your host and typing in the `STARTTLS` command. The output is usually pretty descriptive. For example:

```
220 peculiar.differentpla.net ESMTP
HELO
250 peculiar.differentpla.net
STARTTLS
454 TLS missing certificate: error:0200100D:system library:fopen:Permission denied (#4.3.0)
```

If it's working, you should see the following:

```
220 peculiar.differentpla.net ESMTP
HELO
250 peculiar.differentpla.net
STARTTLS
220 ready for tls
```

For more information, check out [http://iain.cx/ssl/?qmailtls](http://iain.cx/ssl/?qmailtls).
