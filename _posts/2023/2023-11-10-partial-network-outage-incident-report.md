---
title: "Partial Network Outage: Incident Review"
date: 2023-11-10T10:42:00Z
tags: incident-review
---

At approximately 08:45 on November 10th, 2033, there were network connectivity issues on my home network. This is the
incident review.

tl;dr: foxes

## Background

- My home network has a Synology RT2600ac router, with 3 Synology MR2200ac mesh points (wired backhaul) called
  "Kitchen", "Office" and "Loft".
- "Office" is a garden office; it's connected to the main house over a pair of armoured Ethernet cables that run along
  the fence a foot or so above the ground.

## Timeline

All times are in UTC.

November 10th
- 08:45: I noticed that my Electric Imp "environment tail" (which monitors temperature and humidity in my office) hadn't
  reported any readings since approximately 9pm the previous evening.
  - I assumed it was a connectivity problem with the device itself.
- 08:50: Went into the office and rebooted the sensor.
  - Noticed that it was reporting failure while getting the IP address, which implied a problem with the router.
  - Looked at my phone, noticed that I was connected to 4G, rather than Wifi.
- 08:55: Went back into the house to check the router.
  - All the blinkenlights were blinken, so the router itself seemed OK.
  - Phone reconnected to Wifi.
  - Checked the "DS Router" app on my phone. The "Office" access point was down.
  - Grafana showed that my K3s cluster was still connected to the network.
- 09:00: Rebooted the office access point.
  - It did not reconnect.
- 09:05: Realised that the office access point is the only thing currently connected to the second ethernet socket in
  the office.
  - Moved the connection to the office switch (i.e. the first ethernet socket in the office).
- 09:07: **Connectivity was restored.**

## Investigation

- 10:45: Well, there's your problem:

![](/images/2023/2023-11-10-partial-network-outage-incident-report/damaged-cable.jpg)

Foxes have dug under the fence and damaged one of the Ethernet cables running between the house and the office. This is
_just_ at the point where the cable crosses from the fence to the office, so it's lying on the ground.

Fortunately, they've only damaged one of the cables, and fortunately (for the foxes) it's not the power cable (40A is a bit spicy).

## Remediation

Looks like I'm going to need to:

1. Lift the cables away from the ground at this point.
2. Splice in a new connection.
3. Somehow weatherproof the splice.
