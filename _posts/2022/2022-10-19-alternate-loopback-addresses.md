---
title: "Using alternate loopback addresses"
date: 2022-10-19T10:33:00Z
---

Usually, when you use `localhost`, it'll use the `127.0.0.1` loopback address. Did you know that the _entire_
`127.x.y.z` range is reserved for loopback? See [RFC 990][rfc990] and [RFC 3330][rfc3330]. This can be useful sometimes.

[rfc990]: https://www.ietf.org/rfc/rfc990.html#page-6
[rfc3330]: https://www.ietf.org/rfc/rfc3330.html#section-2

## Load testing with multiple connections

One example is if you're running load tests locally. Because each end of an established connection requires two port
numbers, and there are only a limited number of ephemeral ports (~32K on Linux, by default), you'll hit a limit with
only ~16K test connections.

You can increase the limit (depending on OS), but port numbers are 16-bit (i.e. ~64K) and a large chunk of that range is
reserved. That only gives you a little more breathing room.

Or you can bind your client connections to (e.g.) `127.0.0.2`, meaning that you can now have ~32K connections (the
server uses ~32K ports on `127.0.0.1` and the client uses ~32K ports on `127.0.0.2`).

If you want to get clever, you can have your server listen on multiple loopback addresses, have your clients bind to
different loopback addresses, and (somehow; this is the bit where you'll have to get clever) spread them out to the
point that you've got ~256M connections (back of the envelope, so possibly wrong: `127.0.0.0/8` is ~16.7M addresses,
each with ~32K port numbers, divided by 2 for client/server). You'll run out of some other resource before that point,
though.

## Identifying traffic in Wireshark

One other reason you might want to do this is if you're capturing a network trace (in Wireshark, e.g.) and you're trying
to isolate a particular client. In my particular case, I've got a 3-node Kafka cluster running in Docker. Because of the
replication and heartbeating between the nodes, there's a lot of Kafka traffic. Because the cluster traffic uses the
same ports as the client, I can't easily use a filter in Wireshark to only see the client traffic.

On the other hand, if I can persuade the client to bind to `127.0.0.2`, then I _can_ use a filter to see just that
traffic. The exact mechanism for this depends on the client.

<div class="callout callout-secondary" markdown="span">
In my case, I'm using Erlang, so I hacked on the Kafka client library to add `{ip, {127,0,0,2}}` to the options passed
to `gen_tcp:connect`.
</div>

And now I can use `ip.addr==127.0.0.2` in Wireshark to see my client's traffic, and no other.

## macOS, though

On macOS, the `lo0` loopback device is only bound to `127.0.0.1`, so you'll have to fix that first. See [this Superuser
question](https://superuser.com/questions/458875/how-do-you-get-loopback-addresses-other-than-127-0-0-1-to-work-on-os-x),
but (tl;dr):

```sh
sudo ifconfig lo0 alias 127.0.0.2 up
sudo ifconfig lo0 alias 127.0.0.3 up
# ... repeat as many times as needed
```

To remove the aliases:

```sh
sudo ifconfig lo0 -alias 127.0.0.2
# ... and so on
```
