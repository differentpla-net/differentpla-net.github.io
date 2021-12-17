---
title: "qmail STARTTLS is slow"
date: 2006-12-22T11:01:59.000Z
x-drupal-nid: 90
x-needs-review: 2006-12-22T11:01:59.000Z
redirect_from: /content/2006/12/qmail-starttls-is-slow
---
I've recently been struggling to reliably send email via my qmail server. I've got STARTTLS and SMTP AUTH enabled, in order to allow relaying for authenticated users. However, it's been really slow, and Thunderbird quite often times out and gives up on the connection.

I checked that reverse DNS and IDENT checks were turned off on the tcpserver command line; see [http://www.lifewithqmail.org/lwq.html#smtp-slow](http://www.lifewithqmail.org/lwq.html#smtp-slow).

My server's running on a VPS, so I wondered about entropy. TLS uses a lot of random numbers. I found [this thread](http://forums.spry.com/showthread.php?t=209) on the forums of my VPS provider.

Then I went for it and used strace to figure out what was actually going on. Something like this will work:

<pre># svc -d /service/qmail-smtpd
# strace -f -t -osmtpd.strace /var/qmail/supervise/qmail-smtpd/run</pre>

It turns out that the TLS patch uses some precomputed values (according to [this mailing list post from 2004](http://list.elysium.pl/pipermail/smtpauth/2004-March/001211.html)). If these aren't present, it computes them on the fly, causing a substantial slowdown.

The strace output, combined with this information, led me to the solution: my permissions were set wrong on /var/qmail/control/dh1024.pem.

I've got vpopmail installed, and so qmail-smptd isn't running as the qmaild user any more.

So, I simply chown'ed the .pem files appropriately and the problem seems to be fixed.
