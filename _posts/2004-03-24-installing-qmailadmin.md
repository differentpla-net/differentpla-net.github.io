---
title: "Installing qmailadmin"
date: 2004-03-24T18:41:00.000Z
x-drupal-nid: 151
x-needs-review: 2004-03-24T18:41:00.000Z
---
To make it easier to manage the virtual domains on my email box, I've decided to install [qmailAdmin](http://www.inter7.com/qmailadmin.html).

This is Part 14 of [Installing qmail and vpopmail](http://www.differentpla.net/node/view/165).

Before installing it, you need to install some prerequisites. It needs qmail (obviously), vpopmail (which I've already installed) and ezmlm or ezmlm-idx (I've already installed ezmlm-idx). It also needs `autorespond`, which I've not yet installed, so I'd better do that first.

## Installing autorespond

This is as simple as downloading it from [here](http://www.inter7.com/osfree.html), and then:

<pre># tar xvfz autorespond-2.0.2.tar.gz
# cd autorespond-2.0.2
# make
# make install</pre>

It should now be installed in `/usr/local/bin`.

## Configuring qmailAdmin

It's almost your standard `./configure;make;make install` thing, but I need to tweak some of the settings to agree with my installation:

<pre># ./configure --enable-cgibindir=/var/www/flimsy.home.differentpla.net/cgi \
        --enable-vpopuser=vpopmail \
        --enable-htmldir=/var/www/flimsy.home.differentpla.net/html</pre>

I also had to set up the cgi-bin directory properly in the relevant `VirtualHost` block:

<pre><VirtualHost _default_>
    ScriptAlias /cgi-bin/ /var/www/flimsy.home.differentpla.net/cgi/
    ...etc.</pre>

And it works. I can connect to `http://flimsy.home.differentpla.net/cgi-bin/qmailadmin` and I'm presented with the login screen.