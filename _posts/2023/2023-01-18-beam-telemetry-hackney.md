---
title: "BEAM Telemetry: Hackney Metrics"
date: 2023-01-18T20:10:00Z
short_title: Hackney Metrics
tags: erlang
layout: series
series: beam-telemetry
---

In the [previous post]({% post_url 2023/2023-01-15-beam-telemetry-cowboy-metrics %}) of the series, I added basic
metrics reporting to Cowboy, and simply wrote them to the logger. In this post, I'm going to do the same for Hackney,
but write them to Prometheus.

The documentation for Hackney says that [to enable metrics](https://github.com/benoitc/hackney#metrics), you must add a
`mod_metrics` entry to its configuration. Let's put together a small demo app and do that.

Hackney relies on [erlang_metrics](https://github.com/benoitc/erlang-metrics) to abstract away the particular metrics
module you're using. It includes support for `exometer` and `folsom` which are two popular metrics libraries in the
Erlang ecosystem,

The module that you specify in the configuration must implement certain functions.

<div class="callout callout-warning" markdown="span">
The latest version of Hackney uses an old version of `erlang_metrics`. The callback functions have changed a lot in the
newer version. Make sure you're implementing the correct (older) ones.
</span>

```erlang
-module(hackney_metrics_demo_metrics).

-export([new/2, delete/1, increment_counter/1, increment_counter/2, decrement_counter/1,
         decrement_counter/2, update_histogram/2, update_gauge/2, update_meter/2]).

-include_lib("kernel/include/logger.hrl").

new(_Type, _Name) -> ok.
delete(_Name) -> ok.

increment_counter(Name) -> increment_counter(Name, 1).
increment_counter(Name, Value) -> ?LOG_INFO(#{f => ?FUNCTION_NAME, name => Name, value => Value}).
decrement_counter(Name) -> decrement_counter(Name, 1).
decrement_counter(Name, Value) -> ?LOG_INFO(#{f => ?FUNCTION_NAME, name => Name, value => Value}).

update_histogram(Name, Fun) when is_function(Fun, 0) ->
    Begin = os:timestamp(),
    Result = Fun(),
    Duration = timer:now_diff(os:timestamp(), Begin) div 1000,
    ?LOG_INFO(#{f => ?FUNCTION_NAME, name => Name, value => Duration}),
    Result;
update_histogram(Name, Value) when is_number(Value) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME, name => Name, value => Value}).

update_gauge(Name, Value) -> ?LOG_INFO(#{f => ?FUNCTION_NAME, name => Name, value => Value}).
update_meter(Name, Value) -> ?LOG_INFO(#{f => ?FUNCTION_NAME, name => Name, value => Value}).
```

If we make an HTTP request in the shell...

```erlang
hackney:get(SomeUrl).
```

...we get a bunch of metrics:

```
f: increment_counter, name: [hackney,"blog.differentpla.net",nb_requests], value: 1
f: increment_counter, name: [hackney,nb_requests], value: 1
f: increment_counter, name: [hackney,total_requests], value: 1
f: update_histogram, name: [hackney_pool,default,in_use_count], value: 1
f: update_histogram, name: [hackney_pool,default,free_count], value: -1
f: update_histogram, name: [hackney,"blog.differentpla.net",connect_time], value: 77.395
f: increment_counter, name: [hackney_pool,"blog.differentpla.net",new_connection], value: 1
f: update_meter, name: [hackney_pool,default,take_rate], value: 1
f: increment_counter, name: [hackney_pool,"blog.differentpla.net",reuse_connection], value: 1
f: update_histogram, name: [hackney,"blog.differentpla.net",response_time], value: 130.577
```

<div class="callout callout-info" markdown="span">
https://github.com/rlipscombe/hackney_metrics_demo/tree/0.1.0
</div>

I'd like to export these metrics to Prometheus. I'll tackle that in a later post.
