---
title: "Installing vpopmail"
date: 2004-03-11T09:33:00.000Z
x-drupal-nid: 82
x-needs-review: 2004-03-11T09:33:00.000Z
---
Part 4 of [Installing qmail and vpopmail](http://www.differentpla.net/node/view/165).

## Installing vpopmail

vpopmail is available from [http://www.inter7.com/vpopmail.html](http://www.inter7.com/vpopmail.html). The latest version available at the time of this article was 5.4.0, so I grabbed that.

Installation proceeded exactly as laid out in the INSTALL file:

<pre># groupadd -g 89 vchkpw
# useradd -g vchkpw -u 89 vpopmail
# mkdir -p /home/vpopmail
# chown vpopmail.vchkpw /home/vpopmail
# cd /usr/local/src/vpopmail-5.4.0
# ./configure
# make
# make install-strip</pre>

## Configuring the first virtual domain

As mentioned above, `peculiar` handles the email for a bunch of different domains. I'd like to configure each one of these as a different virtual domain. In order to mirror the real system, these are:

*   differentpla.test (for differentpla.net)
*   beerology.test (for beerology.net)

Adding the first domain is easy:

<pre># cd /home/vpopmail/bin
# ./vadddomain beerology.test</pre>

It'll prompt for the postmaster password. For now, I'll just use "password". I'll be using something more secure on the live system.
## Configuring the first virtual user

Adding the first user is also easy:

<pre># cd /home/vpopmail/bin
# ./vadduser roger@beerology.test password</pre>

Again, I'm using "password" as the password.
## Configuring internal DNS

Rather than break my existing setup, I'm going to tweak my internal DNS to point some fake domains at the test box. In /etc/bind/named.conf:

<pre>// Test domains go here

zone "beerology.test" {
        type master;
        file "/etc/bind/db.beerology.test";
};
</pre>

In /etc/bind/db.beerology.test:

<pre>@       IN      SOA     vague.differentpla.net hostmaster.differentpla.net. (
                        200403101       ; serial, todays date+todays serial
                        7200            ; refresh, seconds
                        7200            ; retry, seconds
                        3600000         ; expire, seconds
                        86400 )         ; minimum, seconds

                NS      vague.differentpla.net.

flimsy          A       192.168.0.6

beerology.test. IN MX 10 flimsy.beerology.test.</pre>

Then we can restart bind and test it:

<pre># /etc/init.d/bind stop
# /etc/init.d/bind start
# nslookup
> flimsy.beerology.test
Server:         192.168.0.2
Address:        192.168.0.2#53

Name:   flimsy.beerology.test
Address: 192.168.0.6
> set type=MX
> beerology.test
Server:         192.168.0.2
Address:        192.168.0.2#53

beerology.test  mail exchanger = 10 flimsy.beerology.test.</pre>

## Testing it

So, with vpopmail installed and configured and with DNS configured correctly, I can send an email from my normal address to roger@beerology.test. It turns up in `/home/vpopmail/domains/beerology.test/roger/Maildir` correctly. Hurrah!

## POP3 with vpopmail

Now to configure POP3 access. Again, this is explained in the vpopmail INSTALL file. My `/var/qmail/supervise/qmail-pop3d/run` file looks like this:

<pre>#!/bin/sh
FQDN=`hostname`.`dnsdomainname`
exec /usr/local/bin/softlimit -m 3000000 \
    /usr/local/bin/tcpserver -v -R -H -l 0 0 110 \
    /var/qmail/bin/qmail-popup $FQDN \
    /home/vpopmail/bin/vchkpw \
    /var/qmail/bin/qmail-pop3d Maildir 2>&1</pre>

To test that from Outlook Express, I set up another account as follows:

<table border="1">
<tbody>
<tr>
<td>Display Name:</td>

<td>Roger Lipscombe</td>

</tr>

<tr>
<td>Email Address:</td>

<td>roger@beerology.test</td>

</tr>

<tr>
<td>POP3 Server:</td>

<td>flimsy.beerology.test</td>

</tr>

<tr>
<td>SMTP Server:</td>

<td>flimsy.beerology.test</td>

</tr>

<tr>
<td>User ID:</td>

<td>roger@beerology.test</td>

</tr>

<tr>
<td>Password:</td>

<td>password</td>

</tr>

</tbody>

</table>

A quick push of the "Send/Receive" button and the message that I sent earlier arrives in my Inbox!

## SMTP AUTH with vpopmail

To use `vchkpw` for checking passwords for SMTP AUTH, I had to change my `/var/qmail/supervise/qmail-smtpd/run` file, replacing `/bin/checkpassword` with `/home/vpopmail/bin/vchkpw`.

So that vchkpw can access the password database, I changed the account which qmail-smtpd runs under:

<pre>#!/bin/sh

# QMAILDUID=`id -u qmaild`
# NOFILESGID=`id -g qmaild`
VPOPUID=`id -u vpopmail`
VPOPGID=`id -g vpopmail`
MAXSMTPD=`cat /var/qmail/control/concurrencyincoming`
LOCAL=`head -1 /var/qmail/control/me`

if [ -z "$VPOPUID" -o -z "$VPOPGID" -o -z "$MAXSMTPD" -o -z "$LOCAL" ]; then
    echo VPOPUID, VPOPGID, MAXSMTPD or LOCAL is unset in
    echo /var/qmail/supervise/qmail-smtpd/run
    exit 1
fi

if [ ! -f /var/qmail/control/rcpthosts ]; then
    echo "No /var/qmail/control/rcpthosts!"
    echo "Refusing to start SMTP listener because it'll create an open relay"
    exit 1
fi

exec /usr/local/bin/softlimit -m 3000000 \
    /usr/local/bin/tcpserver -v -R -l "$LOCAL" -x /etc/tcp.smtp.cdb \
        -c "$MAXSMTPD" -u "$VPOPUID" -g "$VPOPGID" 0 smtp \
        /var/qmail/bin/qmail-smtpd \
        /home/vpopmail/bin/vchkpw \
        /bin/true 2>&1</pre>

Testing with Outlook Express shows that this works.

## Adding another domain

To test that the accounts truly are separate, I configured another domain, this one `differentpla.test`, similarly to the above. Nothing exciting happened; it just worked.

Next: [Installing BincIMAP](http://www.differentpla.net/node/view/171)