---
title: "BEAM Telemetry: Prometheus"
date: 2023-01-19T17:58:00Z
short_title: Prometheus
tags: erlang
layout: series
series: beam-telemetry
---

In the previous posts, we gathered metrics from [Cowboy]({% post_url 2023/2023-01-15-beam-telemetry-cowboy-metrics %})
and [Hackney]({% post_url 2023/2023-01-18-beam-telemetry-hackney %}). I'd like to publish the metrics to Prometheus.

<div class="callout callout-info" markdown="span">
I'm actually going to publish the metrics to [VictoriaMetrics](https://victoriametrics.com/), because
I already have that [installed]({% post_url 2022/2022-10-16-victoria-metrics %}), but it's broadly the same.
</div>

To export metrics to Prometheus, your application exposes an HTTP endpoint and that's regularly scraped for the metrics.

We could do this ourselves -- it's just a plain-text format -- but someone's already done it:

- [prometheus.erl](https://hex.pm/packages/prometheus), which collects the metrics to ETS. It's similar to `exometer` or
  `folsom`, which are other metrics libraries for Erlang.
- [prometheus_cowboy](https://hex.pm/packages/prometheus_cowboy); which exports the metrics using Cowboy. If you don't
  want to use Cowboy, there's also an exporter for Erlang's built-in `httpd`, among others.

Out of the box, it reports Erlang VM metrics. There's also an easy integration for reporting cowboy metrics which is an
improvement on [just logging them]({% post_url 2023/2023-01-15-beam-telemetry-cowboy-metrics %}).

If we use it like like this...

```erlang
-define(PORT, 8126).

start(_StartType, _StartArgs) ->
    Dispatch =
        cowboy_router:compile([{'_',
                                [{"/metrics/[:registry]", prometheus_cowboy2_handler, []}]}]),
    {ok, _} =
        cowboy:start_clear(http,
                           [{port, ?PORT}],
                           #{env => #{dispatch => Dispatch},
                             stream_handlers => [cowboy_stream_h]}),
    ?LOG_INFO("Cowboy server listening on port ~p", [?PORT]),
    %...
```

...then we can browse to `http://localhost:8126/metrics/default` and see all of the exported metrics.

## Erlang VM Metrics

Each one looks something like this:

```sh
# TYPE erlang_vm_memory_ets_tables gauge
# HELP erlang_vm_memory_ets_tables Erlang VM ETS Tables count.
erlang_vm_memory_ets_tables 56
```

This one is `erlang_vm_memory_ets_tables`; it's a gauge, described as "Erlang VM ETS Tables count". My VM seems to have
56 ETS tables at the time this sample was taken. This agrees with `length(ets:all())`, so that seems legit.

Unlike graphite metrics, for example, Prometheus metrics can have multiple labels, which provide dimensionality for your
querying. Here's an example of that:

```sh
# TYPE erlang_vm_memory_system_bytes_total gauge
# HELP erlang_vm_memory_system_bytes_total The total amount of memory currently allocated for the emulator that is not directly related to any Erlang process. Memory presented as processes is not included in this memory.
erlang_vm_memory_system_bytes_total{usage="atom"} 1098001
erlang_vm_memory_system_bytes_total{usage="binary"} 228192
erlang_vm_memory_system_bytes_total{usage="code"} 25677934
erlang_vm_memory_system_bytes_total{usage="ets"} 1870616
erlang_vm_memory_system_bytes_total{usage="other"} 18776457
```

This one is the memory usage. See [`erlang:memory/0`](https://www.erlang.org/doc/man/erlang.html#memory-0) for details.

Other metrics might have more than one label. Here's a snippet from `erlang_vm_allocators`:

```sh
erlang_vm_allocators{alloc="binary_alloc",instance_no="6",kind="mbcs",usage="carriers"} 1
erlang_vm_allocators{alloc="binary_alloc",instance_no="6",kind="mbcs",usage="carriers_size"} 32768
erlang_vm_allocators{alloc="binary_alloc",instance_no="6",kind="mbcs",usage="blocks"} 0
erlang_vm_allocators{alloc="binary_alloc",instance_no="6",kind="mbcs",usage="blocks_size"} 0
```

It's saying that the `binary_alloc` allocator, instance 6, kind multi-block carrier (mbcs) currently has 1 carrier,
32KiB in size, with zero allocated blocks.

If you've got multiple instances of your service, you'll want the scraper to add node/pod labels to the metrics when it
imports them. Fortunately, VictoriaMetrics does that automatically. I assume Prometheus (and other scrapers) will do the
same.

## Cowboy Metrics

The `prometheus_cowboy` package also provides an easy way to export cowboy's own metrics. We use `cowboy_metrics_h` as
describe in the [previous post]({% post_url 2023/2023-01-15-beam-telemetry-cowboy-metrics %}), but we can use
`prometheus_cowboy2_instrumenter:observe/1`, rather than writing our own conversion function:

```erlang
  #{env => #{dispatch => Dispatch},
    metrics_callback => fun prometheus_cowboy2_instrumenter:observe/1,
    stream_handlers => [cowboy_metrics_h, cowboy_stream_h]}),
```

Next up: turning the Hackney metrics into Prometheus metrics.
