---
title: "Subtracting dates in Javascript"
date: 2007-04-30T17:04:13.000Z
---
Here's how to find the time 30 minutes ago:

<pre>var THIRTY_MINUTES_IN_MS = 30 * 60 * 1000;

var now = new Date();
var nowMs = now.getTime();

var thenMs = nowMs - THIRTY_MINUTES_IN_MS;
var then = new Date(thenMs);</pre>

The secret is that Date.getTime returns the number of milliseconds since the epoch, and that the Date constructor accepts these millisecond values.