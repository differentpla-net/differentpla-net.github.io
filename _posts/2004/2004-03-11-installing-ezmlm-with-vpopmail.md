---
title: "Installing ezmlm with vpopmail"
date: 2004-03-11T12:17:00.000Z
redirect_from: /node/view/174
tags: qmail
layout: series
series: qmail
---

## Installing ezmlm

`peculiar` is host to several mailing lists, using ezmlm-idx. This needs to continue working under vpopmail.

You'll need to grab `ezmlm-0.53.tar.gz` and `ezmlm-idx-0.40.tar.gz` from [ftp.ezmlm.org](ftp://ftp.ezmlm.org/pub/patches/).

Installation proceeds as described in the the `ezmlm-idx/INSTALL.idx` file:

<pre># cd /usr/local/src
# tar xvfz ezmlm-0.53.tar.gz
# tar xvfz ezmlm-idx-0.40.tar.gz
# mv ezmlm-idx-0.40/* ezmlm-0.53
# cd ezmlm-0.53
# patch < idx.path
# make clean
# make
# make man
# make setup</pre>

This results in ezmlm being installed in the `/usr/local/bin/ezmlm` directory, which is fine with me.
## Creating a mailing list under vpopmail

This is actually relatively easy. `ezmlm-make` should be invoked like this:

<pre># /usr/local/bin/ezmlm/ezmlm-make
ezmlm-make: usage: ezmlm-make [-+] [ -a..zA..Z03..9 ] dir dot local host</pre>

So we just need to create the mailing list inside the vpopmail directories:

<pre># /usr/local/bin/ezmlm/ezmlm-make /home/vpopmail/domains/differentpla.test/list \
    /home/vpopmail/domains/differentpla.test/.qmail-list \
    list differentpla.test
# chown -h vpopmail.vchkpw /home/vpopmail/domains/differentpla.test/.qmail-list*
# chown -R vpopmail.vchkpw /home/vpopmail/domains/differentpla.test/list</pre>

Obviously, add whichever switches you need to `ezmlm-make`.

## Testing

And this works fine. Users can subscribe by sending email to the `list-subscribe@differentpla.test` address, and it all works as expected.
