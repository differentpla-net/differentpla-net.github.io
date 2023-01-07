---
title: "Synology VPN compatibility"
date: 2023-01-07T14:52:00.000Z
tags: synology-srm
---

I recently experimented with using [Tailscale Subnet Router]({% post_url 2022/2022-11-25-tailscale-subnet-router %}) as
a VPN, to allow me to connect to my home network from the pub. It sorta works, but it's been unreliable recently. I'm
going to look at the VPN options that my Synology router gives me.

I've got a Synology RT2600ac which has "VPN Plus Server". I want to connect to it from my Windows laptop and,
occasionally, from my Android phone.

## Server support

Synology VPN Plus Server supports a number of different VPN standards:  IPsec, L2TP, OpenVPN, PPTP, Remote Desktop,
SSLVPN, SSTP, WebVPN.

## Android

Android (at least on my Pixel 5a) only supports IKEv2/IPsec, with MSCHAPv2, PSK or RSA authentication.

I'm using Android 13, so the following
[disclaimer](https://kb.synology.com/en-br/DSM/tutorial/How_to_connect_VPN_server_from_Android) on the Synology
Knowledge Center applies:

<div class="callout callout-info" markdown="span">
L2TP and PPTP are not supported on Android 12 and later; clients will need to connect using OpenVPN instead.
</div>

But: OpenVPN's a hot mess.

## Windows

Windows 11 supports IKEv2, SSTP, L2TP/IPsec (certificates or PSK) and PPTP. According to [How do I connect to Synology's VPN Server via Windows PC?](https://kb.synology.com/en-global/DSM/tutorial/How_do_I_connect_to_Synology_VPN_Server_via_Windows_PC) -- which only goes up to Windows 10 -- I need to:

1. Poke around in the Windows registry.
2. Use either PPTP or L2TP/IPsec.

PPTP's not particularly secure, so I guess that means L2TP/IPsec. I don't like the idea of poking around in the registry.

## Conclusions

{% include alerts/warning.html content="It's all a big pile of garbage." %}

## Options

### Tailscale

Stick with Tailscale and figure out why Tailscale's being flaky.

It occasionally screws up networking (particularly with WSL2) on my Surface Go.

I also couldn't access my DS416, even though it was showing up on the Tailscale UI as recently connected. Since this is
where I've got Tailscale subnet routing set up, this is probably why I couldn't connect to the rest of my network.

My problems today also seem to have coincided with my home network being generally squirrely: my mesh network lost two
of its access points (this is _not_ normal), and at least one device lost Internet connectivity for several hours.

Looking at the usage charts from my ISP, I can see that (based on the traffic profile) my son was online for most of
this period, so that wasn't the problem.

### Wireguard

Run a wireguard server somewhere.

I could probably do this with a Raspberry Pi (I've got several of various vintages kicking around). But I think I'd
prefer to run it in a container.

I _could_ just stick it in my K8s cluster, but that strikes me as too clever -- it introduces too many points of
failure, and I don't think my wife wants to become CNCF-certified just to use the home printer when she's in a coffee
shop.

On balance, I think I'm leaning towards upgrading my NAS to something that can run docker, which I've been considering
for Pi-hole anyway. Then I can run Wireguard there.

## Related Links

- <https://kb.synology.com/en-br/DSM/tutorial/How_to_connect_VPN_server_from_Android>
- <https://kb.synology.com/en-global/DSM/tutorial/How_do_I_connect_to_Synology_VPN_Server_via_Windows_PC>
