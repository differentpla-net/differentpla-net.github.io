---
title: "Removing exim and installing qmail"
date: 2004-03-11T09:26:00.000Z
redirect_from: /node/view/166
---
The second part of a multi-part series on installing [Installing qmail and vpopmail](/node/view/165).

## Removing exim

The first thing to do is to remove exim. Because of Debian's package management system, you can't remove exim without installing another package that provides mail-transfer-agent. Because qmail isn't available as a binary Debian package, this is a little trickier than it needs to be. The answer to this conundrum is in the `equivs` package, so we install that:

<pre>apt-get install equivs</pre>

Then, to create the dummy mail-transport-agent package:

<pre># cd /tmp
# cp /usr/share/doc/equivs/examples/mail-transport-agent.ctl .
# equivs-build mail-transport-agent.ctl</pre>

This results in a `/tmp/mta-local_1.0_all.deb` file being created. We can now install this with `dpkg`:
<pre># dpkg -i /tmp/mta-local_1.0_all.deb</pre>

...which replaces exim with the dummy package.
Be careful while removing exim, otherwise you might find that you've managed to uninstall `at`, `cron` and a bunch of other stuff.

## Installing qmail

This is going to be a pure vanilla [Life with qmail](http://www.lifewithqmail.org/) installation, so you should head over there and read that now. If I do anything different, I'll mention it here.

While creating the user IDs, I notice that the `IDS` script creates the qmail user accounts with `/bin/bash` as a login shell. This ought to be `/bin/true`, so I've changed it with `vipw`.

The `./config` script didn't work, either, so I had to use `config-fast`:

<pre>./config-fast flimsy.home.differentpla.net</pre>

I opted for `Maildir` delivery, using the LWQ script and `control/defaultdelivery`.

## Aside: configuring internal DNS

The `TEST.deliver` file asks that we test local-remote delivery. Unfortunately, this doesn't work: the MX for `differentpla.net` is on my internal network. My router will not correctly forward traffic addressed to it that arrives on its internal interface.

To fix this, I've had to configure internal DNS, so that machines on the internal network refer to machines by their internal IP addresses. I have to configure the MX record correctly as well:

<pre>differentpla.net.   IN MX 10 peculiar.differentpla.net.</pre>

To test it:
<pre>$ nslookup
> set type=MX
> differentpla.net</pre>

## Configuring POP3

This is done in the same way as given in Life with qmail, so go and read that. I didn't do anything different.

Next: [Installing SMTP AUTH with qmail](/node/view/167).
