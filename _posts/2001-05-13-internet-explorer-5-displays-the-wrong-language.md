---
title: "Internet Explorer 5 displays the wrong language"
date: 2001-05-13T23:00:00.000Z
---
Recently, while wandering along to my local [Debian mirror](http://www.uk.debian.org/), I was surprised by the fact that it decided to come up in an Asian script.

After a little thought, I decided that it could be down to broken language negotiation. For those who didn't know, a web browser can send an "Accept-Language" field in its request to the web server. This field contains a list of the languages that the browser is prepared to accept. The server can then choose the closest compatible rendering that it has handy.

This setting is controlled in Internet Explorer, under Tools|Internet Options|General - press the Languages button. Anyway, turns out that IE, had as its only language "English (United Kingdom) [en-gb]". I suspect that this is because I've got Windows 2000 installed for UK English.

Obviously, Debian's UK mirror thinks that Japanese (or whatever it is) is a close match for [en-gb]. Adding "English [en]" and "English (United States) [en-us]" to the list fixed it.
