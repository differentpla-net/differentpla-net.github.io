---
title: Simple job control in bash
date: 2017-08-23 09:03+0000
tags: bash
---

## What?

As part of our system test suite, I need to run a bunch of programs at the same
time, and have them restart if they crash.

I also want them all to die at the same time if I stop the foreground process.

I can't use `monit` or `upstart` or `systemd`, because these aren't system
processes; I'm running them locally (probably from a Makefile). I also spent some time messing around with `forever`, but I couldn't persuade it to run non-node processes.

Let's use bash instead...

## The plan

The plan here is to run a couple of **netcat** instances, listening on different
ports. These will stand in for our real processes.

I'm going to run them _without_ the `-k` (keep listening) switch, because that gives me an easy way to make them exit (by sending them data).

We can send them data with `echo "Hello World!" > /dev/tcp/localhost/9220`.

## The script

```
#!/bin/bash

# When the script exits, kill the current process group.
trap "kill -- -$BASHPID" EXIT

# Run the command in the background.
# If it stops, restart it.
(while true; do
    nc -l 9220
done) &

(while true; do
    nc -l 9221
done) &

# Wait indefinitely (for Ctrl+C).
cat
```

If you want a short delay before restarting the process...

```
(while true; do
    nc -l 9220
    sleep 1
done) &
```

If you want the process to restart only if it exits with a non-zero status:

```
(unless nc -l 9220; do
    sleep 1
done) &
```
