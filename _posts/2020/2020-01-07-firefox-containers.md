---
title: "Firefox Containers"
date: 2020-01-07T21:01:00.000Z
tags: firefox
---
I have more than one Google account, and this confuses Google apps.

For example, if I get sent a calendar invite in my work email, it opens in Google
Calendar in my home account, which fails to find the calendar (because it's a work calendar).

If I were using Chrome, I'd use profiles.

That annoys me because it uses a window-per-profile, which clutters up my taskbar,
and makes it hard to find the browser tab I want. I much prefer a single browser window.

But I'm using Firefox, so I make use of Firefox containers (via the Multi-Account
Containers extension). I use the default container for home, and then I have another
container for each of my other Google accounts.

I also have a separate container just for Facebook, because Facebook.

The Multi-Account Containers extension has the ability to open particular sites in
particular containers. Unfortunately, it can't tell the difference between `mail.google.com`
and `mail.google.com`, so to get around _that_, I've pinned the relevant tabs.

This mostly works well.

Where it fails is that I'd like to ignore my work tabs at the weekend, and that seems
to be hard to do with pinned tabs, 'cos they're pinned.

I haven't figured this out yet.

I've considered having "trampoline" pages that would trigger the Multi-Account
Containers extension to open the correct container and then redirect to the relevant
URL. But the extension apparently only matches on the server name, which makes this
harder: I need a server per trampoline.
