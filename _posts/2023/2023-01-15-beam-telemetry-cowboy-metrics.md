---
title: "BEAM Telemetry: Cowboy Metrics"
short_title: "Cowboy Metrics"
date: 2023-01-15T12:55:00Z
tags: erlang erlang-cowboy
layout: series
series: beam-telemetry
---

Cowboy is probably the most popular HTTP server for the Erlang and Elixir ecosystem. Here's how to get metrics from it.

Note that this post only looks at getting the metrics from cowboy and doesn't cover publishing them anywhere. I'll
discuss that in a later post.

## Hello world app

We'll create a simple "Hello World" app. Start with `rebar3 new` as follows:

```sh
rebar3 new app name=cowboy_metrics_demo
```

Add cowboy as a dependency to `rebar.config` and the `cowboy_metrics_demo.app.src` file, and then update the
`cowboy_metrics_demo_app.erl` file as follows:

```erlang
start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", home_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        http,
        [{port, 8190}],
        #{env => #{dispatch => Dispatch}}
    ),
    cowboy_metrics_demo_sup:start_link().
```

Add `home_handler.erl`:

```erlang
-module(home_handler).
-export([init/2]).

init(Req0, Opts) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    Body = <<"Hello World">>,
    Req = cowboy_req:reply(200, Headers, Body, Req0),
    {ok, Req, Opts}.
```

## Middlewares

<div class="callout callout-info" markdown="span">
This is of historical interest only. I add it for context.
</div>

In cowboy 1.x, we needed to use middlewares to instrument cowboy and add metrics. It would look something like this:

```erlang
% This is cowboy 1.x; this doesn't work in cowboy 2.x.
{ok, _} = cowboy:start_http(http, 100,
    [{port, ?PORT}],
    [{env, [{dispatch, Dispatch}],
     {middlewares, [cowboy_metrics_demo_start, cowboy_router, cowboy_handler]},
     {onresponse, fun onresponse/4}]),
```

The idea is that `cowboy_metrics_demo_start` puts the current time into the `Req` object. Then, in `onresponse/4`, we
can use that and the new current time to work out the elapsed time for the request pipeline. We would usually also do
Apache-style access logging in `onresponse/4`.

We can't just put another middleware (`cowboy_metrics_demo_end`, for example) at the end of the pipeline, because other
middlewares can stop the pipeline deliberately (or crash); cowboy doesn't call the remaining middlewares.

We'd need to do something complicated, like wrap `cowboy_handler`, or write a custom stream handler.

## Metrics

Fortunately, cowboy's got us covered. Cowboy 2.x introduces `cowboy_metrics_h`. See
<https://ninenines.eu/docs/en/cowboy/2.9/manual/cowboy_metrics_h/> for the documentation.

To expose metrics from the cowboy pipeline, update the cowboy options as follows:

```erlang
    {ok, _} = cowboy:start_clear(
        http,
        [{port, 8190}],
        #{
          env => #{dispatch => Dispatch},
          % add the following:
          stream_handlers => [cowboy_metrics_h, cowboy_stream_h],
          metrics_callback => fun metrics_callback/1
        }
    ),
```

`cowboy_metrics_h` also provides `metrics_req_filter` and `metrics_resp_headers_filter` options. The documentation
explains what they _do_, but not what they're _for_. As far as I can tell, they're for sanitising the inputs to
`metrics_callback` -- you might want to remove session cookies, auth headers, etc. It's not clear to me why you can't
just do that in `metrics_callback`, though.

`metrics_callback` is called with [everything you could need](https://ninenines.eu/docs/en/cowboy/2.9/manual/cowboy_metrics_h/).

For this example, let's just report some basics:

```erlang
metrics_callback(_Metrics =
                     #{req := #{method := Method, path := Path},
                       req_start := ReqStart, req_end := ReqEnd, req_body_length := ReqBodyLength,
                       resp_start := RespStart, resp_end := RespEnd, resp_body_length := RespBodyLength,
                       resp_status := StatusCode}) ->
    ?LOG_DEBUG(#{method => Method,
                 path => Path,
                 req_elapsed => ReqEnd - ReqStart,
                 req_body_length => ReqBodyLength,
                 resp_elapsed => RespEnd - RespStart,
                 resp_body_length => RespBodyLength,
                 resp_status => StatusCode,
                 elapsed => RespEnd - ReqStart}),
    ok.
```

The timestamps (`req_start`, etc.) are in Erlang "monotonic time" units. See [this
line](https://github.com/ninenines/cowboy/blob/2.9.0/src/cowboy_metrics_h.erl#L139) in `cowboy_metrics_h`. This is
reported in Erlang [`native` time unit](https://www.erlang.org/doc/man/erlang.html#monotonic_time-0).

On my PC, that's nanoseconds, but it's implementation-dependent. To convert them to microseconds, use `erlang:convert_time_unit(Elapsed, native, microsecond)`, as follows:

```erlang
    ?LOG_DEBUG(#{
                 % ...
                 elapsed_us => erlang:convert_time_unit(RespEnd - ReqStart, native, microsecond)}),
```

<div class="callout callout-info" markdown="span">
The code so far is here: <https://github.com/rlipscombe/cowboy_metrics_demo/tree/0.1.0>
</div>

## What's next?

Writing the metrics to the logger is a _start_, but it's not going to give us any pretty graphs. To do that, we'll need
to integrate with Graphite, or Prometheus, or InfluxDB, or whatever. I'll discuss that in a later blog post.
