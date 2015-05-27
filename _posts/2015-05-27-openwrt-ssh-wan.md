---
title: Enabling remote SSH access on OpenWRT 12.09
date: 2015-05-27 20:15+0100
---

I don't recommend enabling remote SSH access to your router, but if you need
to, here's how to do it via the web interface (LuCI):

## Configure dropbear

1. Go to the System / Administration page.
2. Under "SSH Access", for the default "Dropbear instance", set "Interface" to
   "unspecified".

This will cause dropbear to accept connections on all interfaces.

## Open port 22

You still need to open the relevant port in the firewall.

1. Go to the Network / Firewall / Traffic Rules.
2. Scroll down to the "Open ports on router" section.
3. Enter a name for this rule, e.g. "Allow-SSH-WAN".
4. Set "Protocol" to "TCP".
5. Enter "22" as the "External Port".
6. Click "Add".
7. Click "Save and Apply".

You should now be able to access your router from the WAN side.

## Security notes

If you _are_ going to enable external SSH access, you should conside disabling
SSH password authentication. You'll have to add some public SSH keys.
