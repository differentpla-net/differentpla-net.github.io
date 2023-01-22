---
title: "Erlang Clustering: a survey"
date: 2023-01-22T12:10:00Z
tags: erlang
---

A [question](https://hachyderm.io/@alter_kaker/109713602011411781) on Mastodon asks _"What are people using [for cluster
management] in 2023?"_. I thought I'd address a couple of hidden assumptions in the question and do a quick survey of
what's available.

## Mature or abandoned?

One of the things about the Erlang ecosystem is that it's kinda slow-moving compared to some others -- even compared to
Elixir. It's not unusual to find a package that hasn't had any recent updates and looks to be abandoned. Frequently,
however, this is because it's _finished_. It's stable, mature, and doesn't _need_ any updates.

> **Q**: Age is no guarantee of efficiency.
>
> **James Bond**: And youth is no guarantee of innovation.

-- Skyfall (2012)

It's a risk, of course. You might start relying on it and then discover a bug, and _then_ you'll find out whether it's
actually abandoned. I don't have any solid advice here. You kind of get a feel for the difference after a while.

## What is "clustering"?

When people talk about "clustering", they're usually conflating a number of different topics:

- Node discovery.
- Node communication.
- Process discovery.
- Leader elections.

Erlang/OTP, out of the box, handles node communication [just fine]({% post_url 2022/2022-11-11-erlang-clustering-recap
%}). It also provides some very basic [node discovery](https://www.erlang.org/doc/man/net_adm.html#world-0).

So, what's missing?

## Node discovery

Erlang's node discovery was fine back when everyone was cool with hand-configuring things. These days, we're looking for something easier.

As far as I know, the current state of the art is [libcluster](https://hex.pm/packages/libcluster), about which I've
[written]({% post_url 2022/2022-01-08-libcluster-kubernetes %}) in the past. It's an Elixir package; for an Erlang-only
application, you'll have to put something together yourself.

## Process discovery

Of course, now you've got your _nodes_ talking to each other, you'll need to get your _processes_ talking to each other.
Again, this just works out of the box. Where you're going to struggle is _finding_ the process you want to talk to.
You'll need a process registry.

- Erlang/OTP comes with [`pg`](https://www.erlang.org/doc/man/pg.html), which is used by Phoenix PubSub.
- Still in Erlang-world, there's [`gproc`](https://github.com/uwiger/gproc).
- There's [`syn`](https://github.com/ostinelli/syn), which was originally aimed at the IoT field.
- I've used [`horde`](https://hex.pm/packages/horde) in the past.
- There's also [`swarm`](https://hex.pm/packages/swarm). I don't have a lot of experience with that.

They each make different trade-offs w.r.t. consistency and availability, and they use different cross-cluster strategies: horde uses CRDTs; gproc uses a leader, for example.

## Leader elections

- `gen_leader` -- this one _does_ seem to be in some kind of purgatory. The "latest" version I can find (and the one used by gproc) is this one: <https://github.com/garret-smith/gen_leader_revival.git>.

## Uncategorised

- [`ra`](https://github.com/rabbitmq/ra), which is RabbitMQ's implementation of Raft.
- [`riak_core`](https://github.com/basho/riak_core).
- [`partisan`](https://github.com/lasp-lang/partisan).

What did I miss? Hit the discussion button at the top, or find me on Hackyderm.
