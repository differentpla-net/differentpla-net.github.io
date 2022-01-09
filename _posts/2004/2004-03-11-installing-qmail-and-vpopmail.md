---
title: "Installing qmail and vpopmail"
date: 2004-03-11T09:24:00.000Z
redirect_from: /node/view/165
tags: qmail
layout: series
series: qmail
---
My mail server runs `qmail`. This is the first of a multipart series showing how I went about installing `vpopmail` for virtual domain support.

## Introduction

My webserver, on `peculiar.differentpla.net` is configured for virtual hosting. Among the different websites that it hosts are [differentpla.net](/) and [beerology.net](http://www.beerology.net/).

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

{% include _series_toc.html %}
