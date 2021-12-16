---
title: "Streaming to Winamp"
date: 2001-12-18T22:44:00.000Z
tags: rio-receiver
---
Since the Audio Receiver Manager (the server) uses HTTP to stream the MP3 data to the Rio Receiver, it's possible (with some work) to stream directly to Winamp. Here's how.

For a simple example, we'll play a playlist.

Enter `http://hostname:12078/content/100?_extended=1` into your browser, remembering to replace `hostname` with the name of the machine running the server. Note, however, that `localhost` won't work.

You'll get something like this:

<div class="snippet">
    12cc0=PBanco De Gaia - Big Men Cry
    11740=PBlue Lines
    12cd0=PBoundary Conditions
    117b0=PCafe del Mar Vol 1
    117c0=PCafe del Mar Vol 2

</div>

Each line reads as `fid=PTitle`. The fid is the internal ID of the item in question. The "P" denotes that this is a playlist, and the title is obvious. We'll pick one of these playlists, e.g. "Cafe def Mar Vol 1"

Enter the same query as above, but replacing the "100" with one of the fids in the list. In our example, that will be `http://hostname:12078/content/117b0?_extended=1`

This results in the following:

<div class="snippet">
    7a80=TAgua
    7ae0=TThe Story Of Light
    7ab0=TSmokebelch II (Beatless Mix)
    7a70=TMusic For A Found Harmonium
    7a60=TSundance
    7a30=TFanfare Of Life
    7ac0=TThe Hypnotist
    7a90=TSecond Hand
    7aa0=TCrazy Ivan
    7a40=TEstelle
    7a50=TOn The Rocks
    7ad0=TSunset At The Cafe Del Mar

</div>

These are all tunes (denoted by the "T"). Let's say that we want to listen to "Crazy Ivan". To do this, we simply point Winamp (using the Open URL feature) at `http://hostname:12078/content/7aa0`

And that's it. In next week's instalment: How to use your browser to run queries against the Audio Receiver Manager database.
