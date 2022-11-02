---
title: "Replacing RabbitMQ with nginx for inbound request routing"
date: 2022-11-22T18:45:00Z
tags: electric-imp rabbitmq nginx
---

In a conversation on Twitter about most satisfying code cleanup, I mentioned the time that I completely replaced the
agent-inbound HTTP routing at Electric Imp.

> Mine is replacing some nodejs/RabbitMQ-based RPC thing for routing incoming HTTP requests. We replaced it with nginx
> as a proxy, using a tiny bit of lua to look up the destination in redis. Much simpler. Way more performant and
> scalable.

-- https://twitter.com/rogerlipscombe/status/1574808928835276800

## Caveat

I'm no longer at Electric Imp (now part of Twilio), so this is from memory.

## Background

On the Electric Imp platform, each device has a matching "agent", which runs in the cloud, even when the device is not
connected. That agent can expose an HTTP endpoint.

> Electric Imp applications comprise two parts: code that runs runs on your product hardware (device code) and code that
> runs in the cloud (agent code). Every device has its own, unique agent instance, which acts as the device’s
> intermediary for all Internet communication.

-- https://developer.electricimp.com/software-development-overview

> The unique URL at which a given agent can be contacted via HTTPS. It comprises the base URL agent.electricimp.com plus
> the agent’s ID.

-- https://developer.electricimp.com/faqs/terminology

In the Electric Imp production environment, the running agents are distributed across multiple servers. There are about
10,000-15,000 agents on each server. This meant that we had to route an incoming HTTP request to the correct agent
server, and ensure that the response is sent back to the client successfully.

## Original Implementation

The original implementation of this used an AWS Elastic Load Balancer (ELB), in front of a few instances of the inbound
HTTP service, written in node.js. In order to route to the correct agent server, the HTTP request would be serialized
and published to a RabbitMQ exchange, with the agent ID as the routing key.

Each agent server would subscribe to the RabbitMQ queues corresponding to the agents running on that server. In this
way, the request would make its way to the correct agent, where it could be correctly handled by the customer's code.

When the customer's code needed to send the response back to the client (browser, mobile app, whatever), it would send it back to the originating inbound HTTP service (the response queue would be in one of the RabbitMQ message headers).

This is essentially straight out of
[one of the RabbitMQ tutorials](https://www.rabbitmq.com/tutorials/tutorial-six-python.html), and it's a fairly common
pattern.

It worked pretty well, given our scale/performance/etc. needs at the time, and full props to RichS for putting it
together.

## Problems

It became clear later that there were a few problems with it:

- If the agent never responds, or if the agent crashes, we had to write our own timeout logic.
- If the agent responds late, we need our own logic to discard the unneeded response, since we've already sent a timeout
  response to the client.
- It was difficult to add rate-limiting.
- It made it difficult to diagnose problems.
- As the amount of traffic grew, RabbitMQ became overwhelmed, and harder to manage. To be fair, we were using RabbitMQ
  for a _lot_ of other things at the time, so it could have been something else that pushed it over the edge.
- Because of the simple request/response scheme, it would have been extremely difficult to add streaming (chunked
  encoding) or websockets later. As it happens, we never really got around to it, but it wouldn't have been a good fit
  for a simple request/response mechanism.

## Replacement

After I'd spent some time playing with nginx for something else, I wondered whether we could remove a bunch of the
complexity by using [nginx as a reverse proxy](https://docs.nginx.com/nginx/admin-guide/web-server/reverse-proxy/), and
somehow let it handle the routing to the correct server.

nginx supports scripting with Lua, and you can use it to control the upstream server used in reverse proxy mode, so I
cobbled together the following pieces:

- The [`access_by_lua_file` directive](https://github.com/openresty/lua-nginx-module#access_by_lua_file) allows us to
  run a small piece of Lua for each inbound request.
- That can return a variable to be passed to the [`proxy_pass` directive](https://nginx.org/en/docs/http/ngx_http_proxy_module.html#proxy_pass).

That left the problem of exactly how the Lua would look up the upstream agent server. We were already using redis for a
lot of other things, so I opted to use that, via the [OpenResty redis plugin](https://github.com/openresty/lua-resty-redis).

The agent server writes the agent's location to redis when the agent starts, and removes it when the agent stops.

That required a web server in the agent server that could route the request to the customer's code. We used
[cowboy](https://github.com/ninenines/cowboy/).

This solution has several advantages over the RabbitMQ-based request/response scheme:

- We could easily make use of nginx's built-in rate-limiting functionality, rather than write and maintain our own.
- We got better logging for free.
- The latency was greatly improved.
- We could lean into Erlang's let-it-crash philosophy, because cowboy and nginx would simply do the right thing.

## Related Links

- Someone else is working through it here: https://stackoverflow.com/questions/70657542/dynamic-routing-with-nginx-lua-and-redis
- And here: https://medium.com/@VarshaChahal/using-lua-nginx-module-for-dynamic-routing-based-on-redis-values-1740a10f9905
- And literally in the openresty docs: https://openresty.org/en/dynamic-routing-based-on-redis.html

## Downsides

- The agent servers need to ensure that redis is kept up to date, so if an agent moves, the traffic goes to the correct
  place.
- If the backend process dies, that's not as important, since the cowboy handler can deal with that -- the handler
  returns 404 if the agent isn't running on the current server.
- We ended up needing a redis replica on each nginx node, and had to be careful that those replicas couldn't be promoted.

## Incidents

We misconfigured replication at one point, which led to
[this March 2021 incident](https://status.electricimp.com/incidents/2k6znjp9km84).
