---
title: "Router Upgrade Outage: Incident Review"
date: 2023-01-16T17:38:00.000Z
tags: incident-review
---

On January 16th, 2023 between approximately 03:15 and 08:00, the router on my home network froze. This is the incident review.

## Background

- My home network has a Synology RT2600ac router, with 3 Synology MR2200ac mesh points (wired backhaul) called
"Kitchen", "Office" and "Loft".
- There are wired connections to various other points in the house: living room, bedrooms, etc.

## Timeline

All times are in UTC.

January 13th-15th:
- I plugged in a 256GB USB stick to use as extra storage on the router. I did NOT get around to formatting or
  configuring it.
- The Synology "DS Router" has been notifying me about a pending firmware update for the router and mesh points. This
  was not applied, due to the children having unfinished online homework, thus being unable to negotiate an appropriate
  maintenance window.

January 16th:

- 03:15: The router automatically downloaded and installed the latest firmware upgrade and promptly shit itself. This
  can be seen as a loss of connectivity in the graphs on my ISP's control pages.
- 07:30: As the kids were leaving for school, the wife noticed that the burglar alarm was complaining that it
  couldn't access the Internet.
- 07:35: Investigation started. This required having a shower, getting dressed, and putting some shoes on for the trek
  to the garden office to find a USB/Ethernet adapter and a 6ft Ethernet cable.
- 07:45: Armed with a laptop, USB adapter and cable, I power-cycled the router and waited. The "status" light went from
  yellow to green, but no dice.
- 07:55: Concerned that it might be STP-spam (per the best-guess for the [last outage]({% post_url 2023/2023-01-07-network-connectivity-incident-review %})) or that the router tripped up on
  the new USB stick, I turned off the mesh points, unplugged all of the router's Ethernet cables, except for the uplink
  to the modem and the laptop, pulled the SD card and USB stick, and tried power-cycling again.
- 08:00: Internet connectivity was restored. I reconnected the Ethernet cables to the router and turned on the mesh
  points. I did NOT reinsert the SD card or USB stick.
- 08:15: Normal service restored.

## Metrics

- Time-to-detection: 5h15m.
- Time-to-acknowledge: 15m.
- Time-to-resolution: 45m.

## Things that went poorly

- The laptop had a flat battery, so I needed to plug that in while diagnosing the problem.
- Confusion about the LEDs on the router. It has plenty, but only the "status" LED was doing anything.
- Lack of caffeine.

## Immediate Actions

- I enabled the other LEDs on the router. It would have eased diagnosis if I could see that the Wifi and Ethernet were
  working correctly.

## Root Cause

The idea of a single "root cause" is bullshit. I can think of several things that had to go wrong here:

1. If my suspicions are correct, plugging the USB stick in without formatting it was stupid.
2. Allowing the automatic upgrade to happen on Sunday night was just asking for trouble on Monday morning.

## Further Actions

- Put the USB stick back, but format it and configure it this time.
- Be more assertive with the household Change Approval Board about the need to perform upgrades during normal "working"
  hours.
