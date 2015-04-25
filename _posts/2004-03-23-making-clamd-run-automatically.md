---
title: "Making clamd Run Automatically"
date: 2004-03-23T13:25:00.000Z
x-drupal-nid: 149
x-needs-review: 2004-03-23T13:25:00.000Z
---
Whoops. Back when I was [installing ClamAV](http://www.differentpla.net/node/view/178), I started `clamd` running so that I could test the virus checker. Yesterday, I rebooted the PC.

Obviously, `clamd` didn't restart with the PC, so I was getting "X-Qmail-Scanner-1.21:[flimsy10800482284701388] clamdscan: corrupt or unknown clamd scanner error or memory/resource/perms problem - exit status 2" messages in my log and "qq temporary failure" messages in my email client.

The fix is to steal the startup script for the clamav-daemon Debian package, from [http://packages.debian.org/unstable/utils/clamav-daemon](http://packages.debian.org/unstable/utils/clamav-daemon).