---
title: "Installing ClamAV and Qmail-Scanner"
date: 2004-03-12T16:30:00.000Z
redirect_from: /node/view/178
tags: qmail
layout: series
series: qmail
---
Part 10 of [Installing qmail and vpopmail](/node/view/165). This part talks about installing a virus scanner to check mail before it's delivered.

## Virus Scanner

I'm currently using [McAfee](http://www.mcafee.com/) VirusScan on my Windows XP desktop, but I'd prefer to add virus scanning on the mail server as well. This is for two reasons:

*   Defense in depth
*   McAfee Virusscan breaks Outlook XP when getting mail via POP3 and there's a virus in the email.

I'm going to look at installing [ClamAV](http://www.clamav.net/) on the email server, so that incoming email is scanned for emails before delivery.

## Installing ClamAV

Installation proceeds pretty much as in the instructions:

<pre># groupadd clamav
# useradd -g clamav -s /bin/false -c "Clam AntiVirus" clamav</pre>

It has a few prerequisites, which were missing on this box:

<pre># apt-get install zlib1g-dev libbz2-dev libgmp2-dev</pre>

Compilation should be done as a normal user, so:

<pre>$ cd ~/src
$ tar xvfz clamav-0.67.tar.gz
$ cd clamav-0.67
$ ./configure --sysconfdir=/etc
$ make
$ su -c "make install"</pre>

## Configuring ClamAV

Before you can run ClamAV, you need to edit the configuration file. About the only interesting thing I did here was to put the log file in `/var/log/clamd/clamd.log`, which requires making that directory writable by the `clamav` user.

## Automatic Updates

ClamAV comes with the `freshclam` program, which checks for and downloads updates to the virus database. I configured it to run from `cron`. In `/etc/cron.daily/freshclam`:

<pre>#!/bin/sh

/usr/local/bin/freshclam --quiet</pre>

## Qmail-Scanner

To scan messages as they're queued, I'm going to install [Qmail-Scanner](http://qmail-scanner.sourceforge.net/).

## Installing maildrop

Before installing Qmail-Scanner, I needed to install [Maildrop-1.3.8+](http://download.sourceforge.net/courier/).

<pre>$ tar xvfj maildrop-1.6.3.tar.bz2
$ cd maildrop-1.6.3
$ ./configure
$ make
$ su -c "make install"</pre>

## Installing Qmail-Scanner

First it needs a separate account:

<pre># groupadd qscand
# useradd -c "Qmail-Scanner Account" -g qscand  -s /bin/false qscand</pre>

It also needs a bunch of other stuff installed:

<pre># apt-get install unzip libtime-hires-perl perl-suid</pre>

The installation's a little screwy. You need to run `./configure` once to check that it's figured out what's where on your system, and then run it again as `./configure --install` to actually do the installation. If you miss the second step and try copying the generated file yourself, you'll get "Permission denied" errors.

You'll also need to increase the amount of memory allowed for qmail-smtpd, or you'll see "qq failed: temporary error" messages in your mail client, and "Out of memory!" in `/var/log/qmail/smtpd/current`.

I increased the memory limit to 6Mb or so:

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

exec /usr/local/bin/softlimit -m **6000000** \
    /usr/local/bin/tcpserver -v -R -l "$LOCAL" -x /etc/tcp.smtp.cdb \
        -c "$MAXSMTPD" -u "$VPOPUID" -g "$VPOPGID" 0 smtp \
        /var/qmail/bin/qmail-smtpd \
        /home/vpopmail/bin/vchkpw \
        /bin/true 2>&1</pre>

The installation instructions for Qmail-Scanner also talk about doing some things from a cron job, so I just created `/etc/cron.daily/qmailscan`:

<pre>#!/bin/sh

/var/qmail/bin/qmail-scanner-queue.pl -z
mv -f /var/spool/qmailscan/qmail-queue.log /var/spool/qmailscan/qmail-queue.log.1</pre>

In order to scan incoming and outgoing email, you'll need to change your `/etc/tcp.smtp` file:

<pre>127.:allow,RELAYCLIENT="",QMAILQUEUE="/var/qmail/bin/qmail-queue"
:allow,QMAILQUEUE="/var/qmail/bin/qmail-scanner-queue.pl"</pre>

Don't forget to reload it:

<pre># qmailctl cdb</pre>

Next: [Securing SquirrelMail using HTTPS](/node/view/179).
