---
title: "Network Connectivity: Incident Review"
date: 2023-01-07T16:08:00.000Z
tags: incident-review
---

On January 7th, 2023 between approximately 10:50 and 13:50, there were network connectivity issues on my home network.
This is the incident review.

## Background

- My home network has a Synology RT2600ac router, with 3 Synology MR2200ac mesh points (wired backhaul) called
"Kitchen", "Office" and "Loft".
- There are wired connections to various other points in the house: living room, bedrooms, etc.
- In the cupboard with the router and modem, there is also a Synology DS416play NAS. The NAS runs a [Tailscale subnet router]({% post_url 2022/2022-11-25-tailscale-subnet-router %}).
- The router, mesh points, NAS, are connected to each other and the rest of the house via a Netgear GS108 switch.
  - I should really update the cabling diagram.
- The Ethernet cables for the kitchen and the office run _outside_ the house.
  - It's a 1930's-era cinder-block-built house; running cables is a major pain in the ass (I've _melted drill bits_ when
    trying to drill into the walls), so out-and-back-in is often the best option.
- "Office" is a garden office; it's connected to the main house over a pair of armoured Ethernet cables that run along
  the fence a foot or so above the ground.

## Timeline

All times are in UTC.

January 7th:

- 11:15: I was unable to connect to my home network using Tailscale (subnet routing via my DS416) while sitting in a
  coffee shop.
  - I tried from my phone, and from my tethered laptop.
  - Connecting to various resources on my K3s cluster failed (I was trying to see whether Longhorn had gone healthy
    after this morning's [Ubuntu upgrade]({% post_url 2023/2023-01-07-k3s-apt-upgrade %})).
  - Connecting to the DS416 failed, whether using the split DNS addresses, the Tailscale MagicDNS addresses or the IP
    address.
  - The DS416 was showing as relatively recently connected in the Tailscale UI. Since I don't know how often it checks
    in, this looked good to me.
- 13:50: When I returned home, I attempted to log into the DS416 to investigate the problem, but was unable to.
- Looking on the "DS Router" app on my phone, I noticed that the "Office" and "Kitchen" Wifi points were disconnected.
- I power-cycled the Kitchen Wifi point (since I was in the kitchen at the time); I didn't fancy going to the office to power-cycle that Wifi point (it was raining).
- I looked in the cupboard to check the blinkenlights on the modem, router and network switch. Nothing was obviously
  wrong.
- Everything started working again.

## Further investigation

- None of the family reported having any problems accessing the Internet while I was out.
- I didn't get any notifications from my ISP about lost connectivity. On the other hand, it's the weekend, and I'm not
  sure I opted for 24/7 notifications.
- Looking at the usage charts from my ISP, I can see that (based on the traffic volume) my son was online for most of
  this period, so the problem wasn't with the router or anything Internet-side from it.

In the Log Center on the Synology RT2600ac, on the "Local" page, I can see that:
- The "Office" and "Kitchen" Wifi points were both offline from 10:50.
- They both reconnected at about 13:50.
- All three Wifi points -- "Office", "Kitchen", "Loft" diconnected and reconnected at about 14:45. I suspect that this
  was when I started messing around with some package installation on the router. It was initializing a database on the
  SD card and that severely impacted performance.

On the "Wi-Fi Point" page, I can see the "Kitchen" access point being rebooted at 13:50:30. It took about 15 seconds to
restart.

In the Log Center on the DS416, I notice that between 10:52 and 13:47, LAN 2 repeatedly lost link. It seems to have gone down for about 4 seconds, then come back up for 1m20s and then gone down again. This repeats for seemingly the whole incident time.

## Root Cause

The idea of a single "root cause" is bullshit. There's usually more than one cause.

But in this case, all of the affected devices -- "Office", "Kitchen" and the DS416 -- were connected through the same
Netgear GS108 switch. Of potential interest, the Ethernet cables between this switch and the kitchen and the office all
go along the _outside_ of the house. They could have been affected by the weather (cold, wet) that we've been having
today.

So it could be caused by any (or none) of that. Since it suddenly started working again, who knows?

## Further Actions

- Look to see if there are any signs of damage to the external Ethernet cables.
- Look for evidence of damp incursion into the house.
- Consider upgrading the Netgear GS108 switch to something with better management and monitoring.
  - I suggest this because I suspect something got spammy/noisy and triggered the problem. Having per-port stats/DoS
    protection might be helpful.
- The office is connected with _two_ Ethernet cables. Maybe don't put them both through the same switch...?
  - It won't help much, though -- they're not used for failover (yet); the office is effectively two separate physical
    networks.
  - Investigate bonding options for the two cables. It won't handle every failure scenario: they follow the same route
    from the house to the office.
  - On the other hand (idea!): my K3s cluster is split across those two cables, so did the two halves have difficulty
    talking to each other during this time?
    - Yes. Looking in the Grafana dashboard for Longhorn (e.g.), I see that `roger-nuc1` and `roger-nuc2` stopped
      reporting metrics between 10:51 and 13:49, approximately.
