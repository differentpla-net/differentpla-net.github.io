---
title: "Linux on Sony Vaio - Built-in Wireless"
short_title: "Built-in Wireless"
date: 2003-01-14T15:50:00.000Z
redirect_from: /node/view/157
layout: series
series: linux-on-vaio
tags: linux sony-vaio
---
First, you'll need to recompile your kernel, with the following options:

```
CONFIG_PCMCIA=m
CONFIG_NET_RADIO=y
CONFIG_HERMES=m
CONFIG_PCMCIA_HERMES=m
CONFIG_NET_WIRELESS=y
```

Then, you'll need to make sure you've got the PCMCIA and wireless packages installed: `apt-get install pcmcia-cs wireless-tools`.

Add the following lines to `/etc/modules`:

```
# Load the PCMCIA modules:
pcmcia_core
yenta_socket
ds
```

Then, when you reboot, the correct modules should be loaded:

```
$ /sbin/lsmod
Module                  Size  Used by    Not tainted
orinoco_cs              4520   1
orinoco                28608   0 [orinoco_cs]
hermes                  5312   0 [orinoco_cs orinoco]
af_packet               8392   3 (autoclean)
eepro100               18604   1 (autoclean)
mii                     2320   0 (autoclean) [eepro100]
ds                      6464   2 [orinoco_cs]
yenta_socket            8704   2
pcmcia_core            32992   0 [orinoco_cs ds yenta_socket]
thermal                 6632   0 (unused)
processor               8664   0 [thermal]
fan                     1576   0 (unused)
button                  2420   0 (unused)
battery                 5960   0 (unused)
ac                      1832   0 (unused)
unix                   13572  55 (autoclean)
```

You'll need to edit the `/etc/pcmcia/wireless.opts` file as well. Find the section entitled "Lucent Wavelan IEEE (+ Orinoco, RoamAbout and ELSA)". Fortunately, it's the first proper section in the file. Edit it like this:

```
# Lucent Wavelan IEEE (+ Orinoco, RoamAbout and ELSA)
# Note : wvlan_cs driver only, and version 1.0.4+ for encryption support
*,*,*,00:60:1D:*|*,*,*,00:02:2D:*)
    INFO="Sony Vaio Orinoco card"
    ESSID="essid"
    MODE="Managed"
    KEY="0123456789"
    ;;
```

Obviously, put the correct values for your wireless network in the `ESSID` and `KEY` variables. Put anything you want in
the `INFO` variable. You'll also need to add the following line to `/etc/network/interfaces`:

```
iface eth1 inet dhcp
```

Without this line, you'll get a message "Ignoring unknown interface eth1=eth1."
Then, check to see if your wireless networking comes up by:

```
/etc/pcmcia# ./network start eth1
```

Use `ifconfig` to confirm that it's working.

## Other Resources

- <http://ozlabs.org/people/dgibson/dldwd/>
