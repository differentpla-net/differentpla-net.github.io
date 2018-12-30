---
title: "Installing SMTP AUTH with qmail"
date: 2004-03-11T09:28:00.000Z
x-drupal-nid: 81
x-needs-review: 2004-03-11T09:28:00.000Z
---
This is the third part in a multipart series on [Installing qmail and vpopmail](/node/view/165)

## Installing SMTP AUTH

In the Requirements list, above, I talked about being able to send email from anywhere, using my laptop. This is known as selective relaying.

There are several ways to allow selective relaying. The easiest is useful if all of the machines to send mail are on the local network. We just add a line like the following to `/etc/tcp.smtp`:

<pre>192.168.0.:allow,RELAYCLIENT=""</pre>

I'm not going to do this yet, because I'm doing all of my testing from my internal network, so I'll not know if I've correctly configured relaying for my laptop until it's too late.
Another way to allow selective relaying is SMTP-after-POP. Essentially, authenticating with the POP daemon adds you to the tcprules file for a limited time, allowing relaying. Most modern email clients can be configured to work like this. For example, in [Outlook XP](/images/946aa447a66295f55baa9ba0a0ebf32f-169.jpg), it's called "Log on to incoming mail server before sending email".

The first thing that we need to do is test that it doesn't allow relaying by default. To do this, we'll set up a new account in Outlook Express (because I'm already using Outlook for my real email on my desktop PC) that tries to use `flimsy` as the SMTP server. If we attempt to send email like this, we should see something like the following:

<pre>553 sorry, that domain isn't in my list of allowed rcpthosts (#5.7.1)</pre>

There's a combined TLS+SMTP Auth [patch](http://shupp.org/patches/netqmail-1.05-tls-smtpauth-20040207.patch) listed on [qmail.org](http://www.qmail.org/netqmail/). It works with netqmail-1.05\. Instructions are in the top of the patch.

For this patch, you'll also need the OpenSSL binaries and libraries:

<pre>apt-get install openssl libssl-dev</pre>

One problem I had after installing this patch was that the SMTP connection would be closed abruptly. Looking in the logs revealed the following:

<pre>/var/qmail/bin/qmail-smtpd: error while loading shared libraries:
libc.so.6: failed to map segment from shared object: Cannot allocate memory</pre>

LWQ [says](http://www.lifewithqmail.org/lwq.html#supervise-tree) to increase the `-m` parameter to `softlimit` to 3000000 (around 3Mb). This fixes the problem.
You'll also need a [checkpassword](http://cr.yp.to/checkpwd.html) program.

Don't forget to read the `README.auth` in `/usr/local/src/netqmail-1.05/netqmail-1.05` for information on installing it correctly.

In particular, if you get `503 auth not available (#5.3.3)` messages, you probably forgot to add /bin/checkpassword to /var/qmail/supervise/qmail-smtpd/run. Look in README.auth again.

Another problem is that, by default, `/bin/checkpassword` is installed with over-strict permissions. `qmail-smtpd` can't actually run it in the default configuration. To fix this, we need to change the permissions and make it setuid root.

<pre># chown root.root /bin/checkpassword
# chmod 4755 /bin/checkpassword</pre>

This is a potential security hole. I'm going to install vpopmail later, which has its own checkpassword program. When I've installed the replacement, I'll restore checkpassword to its original settings (or, in fact, delete it, since nothing will be using it).
If we then [turn on SMTP AUTH](/images/ab44157cb410409dc6a8033aeb7fccf3-168.jpg) in Outlook Express, it works. I can send mail from my Windows PC to another host. If I quickly test it by turning off SMTP AUTH in Outlook Express, it fails, as expected.

Next: [Installing vpopmail](/node/view/170).
