---
title: "Installing qmail and vpopmail"
date: 2004-03-11T09:24:00.000Z
x-drupal-nid: 70
x-needs-review: 2004-03-11T09:24:00.000Z
---
My mail server runs `qmail`. This is the first of a multipart series showing how I went about installing `vpopmail` for virtual domain support.

## Introduction

My webserver, on `peculiar.differentpla.net` is configured for virtual hosting. Among the different websites that it hosts are [differentpla.net](http://www.differentpla.net/) and [beerology.net](http://www.beerology.net/).

It also manages email for those domains. Unfortunately, the email system is not set up to allow virtual hosting. This means that any email to (e.g.) fred@differentpla.net gets delivered to the same place as fred@beerology.net. Now, this might not be a problem if the user accounts referred to the same person. Even here, fred might want to keep the two accounts separate.

`peculiar` is running qmail to handle email. Since it's running my "production" email, I'm going to install a test box to try all of this out on.

## Requirements

This is what I want out of the mail server when I'm finished:

*   Separate virtual domains, so that fred@differentpla.net and fred@beerology.net are delivered to different mailboxes.
*   POP3 support, so that my girlfriend can download her email to the desktop PC.
*   SMTP AUTH support, so that I can send mail from my laptop from anywhere.
*   IMAP support, so that I can access my mail from my laptop anywhere.
*   Forwarding. A couple of user accounts on this box are forwarded elsewhere. This must continue to work.
*   Mailing lists (using ezmlm).
*   Webmail, so that I can access my mail without my laptop from anywhere.
*   Virus/spam filtering.

## The test box

Because `peculiar` is currently handling my "production" email, I don't want to accidentally break it while trying out all of this new stuff, so I'm going to set it up on a test box.

For this, I've dug up my girlfriend's old PC (a Pentium-200), which should be adequate for this test system. On it, I've installed Debian GNU/Linux v3.0 (woody), which is the same version as running on `peculiar`. The machine is called `flimsy`.

## The articles

*   Part 1: This page.
*   Part 2: [Removing exim and installing qmail](http://www.differentpla.net/node/view/166).
*   Part 3: [Installing SMTP AUTH with qmail](http://www.differentpla.net/node/view/167).
*   Part 4: [Installing vpopmail](http://www.differentpla.net/node/view/170).
*   Part 5: [Installing BincIMAP](http://www.differentpla.net/node/view/171).
*   Part 6: [Using qmail extension addresses with vpopmail](http://www.differentpla.net/node/view/172).
*   Part 7: [Forwarding addresses to another account using vpopmail](http://www.differentpla.net/node/view/173).
*   Part 8: [Installing ezmlm with vpopmail](http://www.differentpla.net/node/view/174).
*   Part 9: [Installing SquirrelMail](http://www.differentpla.net/node/view/175).
*   Part 10: [Installing ClamAV and Qmail-Scanner](http://www.differentpla.net/node/view/178).
*   Part 11: [Securing SquirrelMail using HTTPS](http://www.differentpla.net/node/view/179).
*   Part 12: [Securing IMAP](http://www.differentpla.net/node/view/190).
*   Part 13: [SMTP over TLS](http://www.differentpla.net/node/view/196).
*   Part 14: [Installing qmailAdmin](http://www.differentpla.net/node/view/198).