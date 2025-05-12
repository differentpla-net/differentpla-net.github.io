---
title: "Docker (and podman), macOS, localhost"
date: 2025-05-12T16:35
tags: docker podman containers macos
---

On macOS, both Docker and Podman run a Linux VM. This has some weird effects when running (or writing) your own
container registry.

_tl;dr_ use `host.docker.internal`, not `localhost`.

`docker image pull localhost:5000/ubuntu` _doesn't_ talk to the host; it talks to the docker VM. If you're not running
the registry on that VM (because you're writing your own registry, or you inadvertently ran the registry in podman
instead), it will fail to connect, and you'll see nothing in Wireshark if you're capturing the loopback interface.

On the other hand, `podman image pull localhost:5000/ubuntu`, _does_ talk to the host, and you'll see _something_ in
Wireshark on the loopback interface. But what you're seeing is actually SSH tunnel traffic.

Of note: if you're mixing Docker and Podman (running the registry in docker and using `podman pull` or running the
registry in podman and using `docker pull`), they won't be able to talk to each-other, because they're running in
separate Linux VMs.

What you _want_ is `docker pull host.docker.internal:5000/ubuntu`, which will talk to the Mac host, and _that_ will
port-forward to the registry. And then you'll see the traffic in Wireshark.

Persuading Wireshark to decrypt the HTTPS traffic, or getting Docker or Podman to use HTTP? That's left as an exercise
for the reader.