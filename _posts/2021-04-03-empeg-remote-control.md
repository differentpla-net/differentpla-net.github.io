---
title: "Remote-controlling an empeg"
date: 2021-04-03T15:11:00Z
tags: empeg
---

Because ... reasons, I find myself needing to programmatically remotely control
my empeg. It turns out that -- if you've got the Hijack kernel installed -- you
can do this with curl.

## Button Presses

For example, the following presses the topmost button, allowing you to pause and
unpause playback:

```bash
curl "http://${EMPEG_IP}/proc/empeg_notify?button=Top"
```

You can add `.L` for a long-press; the following turns the empeg off:

```bash
curl "http://${EMPEG_IP}/proc/empeg_notify?button=Top.L"
```

## Symbolic Button Names

They're documented at http://www.riocar.org/FAQ/11/196.html.

Unfortunately, some of the button names seem to be really promising, but they're
just aliases for the same underlying button.

For example `Play` and `Pause` sound like they would force the player to be playing or paused, but they're just aliases for the same play/pause action, which toggles the state. Similarly, "Cancel" and "Mark" are aliases for the same action, so, while it _does_ cancel out of the menu, once you're at the top level, it marks the currently-playing track for later attention.

## Examples

Press down-down-down, which causes the empeg to begin playing everything.

```bash
curl "http://${EMPEG_IP}/proc/empeg_notify?button=Menu"
curl "http://${EMPEG_IP}/proc/empeg_notify?button=Menu"
curl "http://${EMPEG_IP}/proc/empeg_notify?button=Menu"
```

TODO: Find a way to cancel out of the menus, so that we know we're at the top. I guess that rebooting (and then somehow waiting until the player's ready) is the best we can manage...?

I did see something in the (text) serial protocol that might do it.

TODO: Is there a way to not start playing immediately?

## Reading playlists and tracks

You can query the empeg with the `?fid=XXXX` parameter. The following retrieves
the root playlist.

```bash
curl "http://${EMPEG_IP}?fid=100"
```

The following gets the tags for the specified track:

```bash
curl "http://${EMPEG_IP}/?fid=151"
```

You can get it in XML format by adding `&ext=.xml`. Note that JSON is not supported.

All of the above could equally well be done by downloading the database and playlist files,
or by looking directly in the `fids` directories.

## Current running order

Where this comes into its own is that you can download the current running order
with the following:

```bash
curl "http://${EMPEG_IP}/?fid=001&ext=.xml"
```

You *must* specify `&ext=.xml`. Plain text (or binary) is not supported.

See https://empegbbs.com/ubbthreads.php/topics/301603/Hijack_v478:_%22get_running
for the relevant release announcement.

Note that because this is read from the dynamic data partition, it's not
necessarily updated immediately.

That's an understatement. I'm 11 tracks into a new running order, and it's
still returning the previous one. It's not clear to me when the player decides to
flush the dynamic data partition. I wonder if there are any disk-spin-up hints
that'll cause it to flush more quickly.

Without a way to do that cleanly, this might be a complete dead-end.

...wonder if it's possible to poke around in the player's memory...?

It's not clear to me how we figure out how far through the running order we are.
It seems to be the `Notify_Track` entry in `/proc/empeg_notify`. Note that it returns
the tracks in shuffled order, but I can't see a good way to get the unshuffled data.

I wonder if there's a way to get at the raw partition information from Hijack's
HTTP or FTP interfaces? Specifically, can we parse it from there? Do we need to?

## Current track

To query the current track, you can just look in `/proc/empeg_notify`, but note
that there are some `config.ini` prerequisites. See http://www.riocar.org/FAQ/11/182.html#182,
but basically you need to set the following:

```
[output]
notify=1
```

Note that the above also writes the output to the serial port, if you want to disable that:

```
[hijack]
suppress_notify=1
```

## Remote Display

The screen of the empeg is available at `/proc/empeg_screen.png`, thusly:

```
curl "http://${EMPEG_IP}/proc/empeg_screen.png"
```

It's a transparent PNG, where only the foreground pixels are set.

This causes both Chrome and Firefox to overlay it on a background that
makes it hard to see.

## Rebooting

You can reboot the empeg with the following:

```bash
curl "http://${EMPEG_IP}/proc/empeg_notify?reboot"
```
