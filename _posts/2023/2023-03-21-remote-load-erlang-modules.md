---
title: "Remotely loading Erlang modules"
date: 2023-03-21T18:44:00Z
tags: erlang
---

Sometimes, you're connected to another node in an Erlang cluster, and you need to inject a new or replacement module.
Here's how.

## Motivation

At Electric Imp, we used "blue/green" deployments: we bring up new servers and once we're sure they're working, we send
the traffic to them. However, because IoT device connections are potentially long-lived, and because we didn't just want
to drop the connections (because reasons), we needed a way to gently disconnect devices from one of the old servers and
send them to the new servers.

Then we realised that we hadn't built that mechanism into the currently-deployed servers. Which was a problem.

Obviously, using Erlang, you can load new code at runtime. But, rather than pushing the new module to each old server
(using scp), and then using the remote console (probably via tmux) to hot-load it, we wondered if there was a better
way?

One of us noticed that Erlang allows you to load a module as a binary, which means that it doesn't need to be a file on
the server's disk. And that you can do this from a remote node.

We wondered: could we just build this into the deploy script, so that it would simply inject the needed deployment
module into the old servers? Every time, so we never needed to worry about compatibility between old and new?

Turns out: yes. So we did. In hindsight, there are probably better ways to do what we did, but having discovered that we
_could_, we didn't stop to think if we _should_.

Whatever. We're not in Jurassic Park. Here's how:

## The code

```erlang
% Load the module locally.
{module, Mod} = code:load_file(Mod).
{Mod, Bin, File} = code:get_object_code(Mod).

% Load the object code into the remote node.
{module, Mod} = rpc:call(Node, code, load_binary, [Mod, File, Bin]).
```

Yeah, that was a bit of an anti-climax after that build-up. Sorry.
