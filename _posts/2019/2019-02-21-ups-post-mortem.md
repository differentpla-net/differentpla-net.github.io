---
title: "A UPS post-mortem"
date: 2019-02-21T10:48:07Z
tags: post-mortem incident-review
---

This morning, something electrical went wrong in my "network cabinet" (hall
cupboard), which knocked me offline for about an hour. Here's the post-mortem.

It turns out that certain people have been putting the vacuum cleaner in the
cupboard without due care and attention.

This caused the UPS to be repeatedly bumped against the back of the cupboard,
putting strain on the cables. This caused the one of the cables (from the UPS
to a power strip) to be partially dislodged.

That, in turn, caused some arcing in the connector which (a) has potentially
been triggering the "UPS offline" emails I've been seeing sporadically over the
last few weeks, and (b) melted the plastic housing slightly, and (c) caused
this morning's household outage. It might also explain why both of my NASes
were off yesterday morning.

Fortunately -- because that's how I roll -- I had some spare IEC-60320-C14
(reverse kettle) cable lying around, so I rewired the power strip with a less
melted connector.

And then I fixed a shelf to the wall and put the UPS on that, so that it's raised
off the floor, where the vacuum cleaner can't get at it.
