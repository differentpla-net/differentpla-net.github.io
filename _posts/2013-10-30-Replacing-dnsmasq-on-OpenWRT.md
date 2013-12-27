---
layout: post
title: "Replacing dnsmasq on OpenWRT"
date: 2013-10-30T11:34:10.764Z
tags: openwrt
alias: /post/UnDvbDtEQN4qAAAB/replacing-dnsmasq-on-openwrt
---

## Overview

My home router runs [OpenWRT](https://openwrt.org/), which uses **dnsmasq** by
default. However, I want to enable [split-horizon
DNS](http://en.wikipedia.org/wiki/Split-horizon_DNS), so that different clients
get different IP addresses for particular names.

This isn't a feature of dnsmasq, so I need to install **dhcpd** and **bind**
instead of **dnsmasq**.

**Note:** I have a guest WiFi network configured; this is the one that I'll be
messing around with, but it shouldn't make any major difference to the
following.

This is how I replaced **dnsmasq**; I'll blog about configuring BIND's DNS
views later.

## Connect using ssh

    ssh openwrt-01

## Removing dnsmasq

    /etc/init.d/dnsmasq stop
    opkg remove dnsmasq

## Installing dhcpd

At the moment, I'm not running IPv6 -- I'll get to it at some point -- so I
chose to install the IPv4-only build of the ISC DHCP server:

    opkg install isc-dhcp-server-ipv4

### Configuring dhcpd

By default, **dhcpd** is configured to hand out addresses in the `192.168.1.1`
range. This doesn't coincide with any of my configured interfaces, which means
that I get the error message:

    No subnet declaration for br-lan (10.0.0.140).
    ** Ignoring requests on br-lan.  If this is not what
       you want, please write a subnet declaration
       in your dhcpd.conf file for the network segment
       to which interface br-lan is attached. **

    No subnet declaration for wlan0-1 (192.168.58.1).
    ** Ignoring requests on wlan0-1.  If this is not what...


    Not configured to listen on any interfaces!

To fix this, I edit `/etc/dhcpd.conf`, changing the `192.168.1.0` section
appropriately.

I **don't** want **dhcpd** answering requests on the `br-lan` interface; this
is my internal network, and there's a Windows Server already handling DHCP and
DNS for that network.

Note that `wlan0-1` is my guest WiFi interface; my internal WiFi interface is
bridged to the `br-lan` interface.

## Installing bind

The next step is to install BIND, to handle DNS lookups:

    opkg install bind-server

## Configuring bind -- forwarders

At the very least, we need to configure BIND to forward requests it doesn't
understand to a normal DNS server.

Because this is my guest network, I'm going to use my ISPs name servers here,
rather than my internal Windows DNS server.

To do this, edit the `/etc/bind/named.conf` file, and uncomment the
`forwarders` section. Put your ISP's name servers in here.

## Configuring bind -- authoritative domains

This is the point at which you'd configure BIND for the domains that you want
it to be authoritative for.

For details, read the BIND documentation; it's quite involved.

## Cleaning up

Definitely:

    rm /var/dhcp.leases         # dhcpd uses /var/dhcpd.leases, with a different format.

Optionally, but this might break LuCi:

    rm /etc/config/dhcp
    rm /var/etc/dnsmasq.conf    # automatically generated from /etc/config/dhcp
