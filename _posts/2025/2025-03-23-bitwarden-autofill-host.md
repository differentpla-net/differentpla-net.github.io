---
title: "Bitwarden Auto-fill and simple hosts"
date: 2025-03-23T13:33:47Z
tags: bitwarden
---

I use Bitwarden for managing my passwords. When using the Firefox browser extension, it refused to autofill the
credentials for my Synology NAS. Here's how I fixed it.

My Synology NAS is at `https://diskstation:5001`. When I browse to it, Bitwarden doesn't recognise the site, so it
doesn't offer to auto-fill the username and password. No matter how many times I selected "Autofill and Save", Bitwarden
never seems to remember the site. I always have to search for the relevant entry and copy the username and password
explicitly.

I finally looked into it this weekend.

It turns out that the default setting for auto-fill's match detection is "Base domain". It seems like this breaks in the
face of host-only URIs, such as the one I'm using with my NAS.

To fix it:

1. Search for the relevant login entry and click _Edit_.
2. In the _Auto-fill options_ section, click the cog (settings) icon next to the first URI.
3. Set _Match detection_ to "Host".
4. Save your changes.

The browser extension now correctly matches the URI (so I get the little "1" badge on the icon), and the "Fill" button
appears and works.
