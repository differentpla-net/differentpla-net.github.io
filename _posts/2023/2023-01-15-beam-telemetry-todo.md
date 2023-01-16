---
title: "BEAM Telemetry: TODO"
date: 2023-01-15T12:50:00Z
short_title: TODO
tags: erlang
layout: series
series: beam-telemetry
---

I've been looking at "observability" recently. Or is it "telemetry"?

Datadog, for example, [talks about](https://www.datadoghq.com/three-pillars-of-observability/) the "three pillars of
observability": metrics, traces and logs. Charity Majors of Honeycomb [rejects that
framing](https://www.honeycomb.io/blog/observability-5-year-retrospective) and says that "observability" is so much
more. The OpenTelemetry [home page](https://opentelemetry.io/) talks about "metrics, logs, and traces" but just calls it
"telemetry".

Let's agree to use the word "monitoring" to describe what I'm looking at.

The plan for this blog series is to look through an Erlang-shaped lens at the three pillars of monitoring. My goal is to
understand OpenTelemetry and how it fits into the Erlang ecosystem.

There's a lot of products, tools, documentation, etc., out there. I suspect that the series is going to meander around until I think I've got a handle on the complexity.

Mind map:


## Metrics

- cowboy metrics handler, with simple logging, mention folsom/exometer?

 need to
know a bit more about the environment the code's going to be used in.

For example, if we were using graphite, we could publish the metrics by using
[`exometer`](https://github.com/Feuerlabs/exometer/) with its [graphite
reporter](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#exometer_report_graphite).

It might look something like this:

```erlang
exometer:update([cowboy, elapsed_us], erlang:convert_time_unit(RespEnd - ReqStart, native, microsecond)).
```

## Publishing metrics to prometheus

We'd probably prefer to publish the metrics to Prometheus. Prometheus allows us to attach labels to our metrics (such as
the request path, or the server name). This allows us to slice-and-dice our metrics in a number of different dimensions,
giving us more visibility into what's going on in our system.

- https://github.com/deadtrickster/prometheus.erl just deals with the metrics; we can write our own exporter.
- But: https://github.com/deadtrickster/prometheus-cowboy exists
  - it includes both the exporter (for Cowboy 1.x and 2.x), _and_ a metrics callback implementation for Cowboy 2.x.

- Can I poll for cowboy metrics (ranch pool sizes, etc.)?


- lead into prometheus.erl
- ranch? metrics
- poolboy? metrics
- something else, maybe?
- hackney metrics thing -- starts to move in the correct direction.
- wouldn't it be nice if there was a homogenous way to report metrics? enter telemetry.
- revisit cowboy telemetry
  - https://hex.pm/packages/opentelemetry_cowboy
- Metrics
- collectd, statsd, datadog
- graphite/carbon/grafana
- folsom, exometer, custom (mecs).
- Erlang VM stats.
- Adding stats for other libraries: poolboy, etc. mecs has plugins for all that crap; we had to do something similar at Electric Imp.
- prometheus
  - prom has tags, graphite doesn't.
  - Erlang VM stats, again.
- different metrics types
  - counter, gauge, histogram, etc.
- hooking logger to measure error, warning, events, since that's how they come out of the Erlang VM.
- can we get stats out of other bits of Erlang?
  - memory, ets, etc., are covered by prometheus.erl (and mostly by the others).
  - what about inet metrics? what about httpd metrics?
  - what about process queue lengths?
- metrics/events for other things in general: poolboy, e.g. what other popular libraries are there?
- can we use erlang trace ids as span ids? how do they even work anyway?
- telemetry: pre-sampling, post-sampling, mix?

Actually, why _does_ cowboy_telemetry_h wrap cowboy_metrics_h? Can't you just get cowboy_metrics_h to call out?

Phoenix default project has

```
{:telemetry_metrics, "~> 0.6"},
{:telemetry_poller, "~> 1.0"},
```

Later:
    - cowboy_telemetry is a separate library; it wraps cowboy_metrics_h and converts the metrics into telemetry events. See https://github.com/beam-telemetry/cowboy_telemetry/blob/main/src/cowboy_telemetry_h.erl#L15.
      - wrapping it is an interesting choice; I'd probably have been more explicit.

## Logging
  - Lager.
  - The new stuff.
  - Compare with Elixir's logger.
  - syslog/rsyslog.
  - 12 factor app (and K8s) just say to write to stdout, let other layer deal with it.
    - K8s log truncation.
    - What ways are there to ship the logs elsewhere?
  - Fluentd.
- things like Splunk, Scalyr, etc.
  - Twilio publishes to Bigtable -- great for queries, not so good for live. Expensive.

## Uncategorized

- 10 years ago, the state of the art was statsd/collectd/graphite
- then we added grafana
- then influxdb
- then we went with prometheus
- Can I use "observability" until I've played with honeycomb?
- Telemetry - probably tackle after metrics.
- OpenCensus vs. OpenTelemetry
- `telemetry` is a way to provide a ubiquitous interface for different libraries to emit telemetry. It's not related to OTel (until you want it to be).
  - if you've got folsom or exometer, how do you tell the library (cowboy, hackney, etc.) to use that metrics library?
    - https://github.com/benoitc/hackney#metrics
    - https://ninenines.eu/docs/en/cowboy/2.9/manual/cowboy_metrics_h/
    - cowboy_telemetry is a separate library; it wraps cowboy_metrics_h and converts the metrics into telemetry events. See https://github.com/beam-telemetry/cowboy_telemetry/blob/main/src/cowboy_telemetry_h.erl#L15.
      - wrapping it is an interesting choice; I'd probably have been more explicit.
      - how _do_ you use cowboy_metrics_h anyway?
        - See this https://github.com/deadtrickster/prometheus-cowboy#exporting-cowboy2-metrics
        - prometheus-cowboy does two (semi-related) things:
          1. a cowboy handler to export prometheus metrics from prometheus.erl.
          2. the above is how to export cowboy metrics from cowboy_metrics_h to prometheus.erl.
    - telemetry =/= metrics; cowboy reports telemetry now, so what's the difference?
    - https://medium.com/the-realreal-product-tech-blog/monitoring-hackney-connection-pools-with-telemetry-3aaeafa8eeb8
- if I'm pushing OTel (or is it pull?), can I publish metrics from _there_, rather than from my daemon?
- likewise: what OTel is there for host/node metrics?
- what OTel stuff is there for K8s? In terms of exporters, I mean.
- jaeger; what other options for that?
- alerting -- on metrics, on other?
- retag lager posts?

- but we want OTel instead?
  - that's a thing as well. it wraps cowboy_metrics_h.
  - what if we want both? out of scope?
  - is there a `telemetry`-to-prometheus.erl connector?
    - isn't this getting a little bit circular?
  - do I need to draw some boxes?

But, again, compare these custom metrics vs. `telemetry` as a standard. Oh, and link to the Erlang Solutions blog post on this stuff.

Another comparison: hackney uses erlang-metrics, which is benoit's (?) attempt to make metrics callbacks generic.
Compare the metrics callbacks in ei_cache.

Should he have used telemetry? There's an open issue which talks about hooks, which would be kinda similar to cowboy's
handlers, and you could cook up your own telemetry handler.

Actually, why _does_ cowboy_telemetry_h wrap cowboy_metrics_h? You don't need to do that. We didn't do that at Electric
Imp, iirc -- we just wrote pre-/post- handlers. Maybe that doesn't work in Cowboy 2.x; we were on Cowboy 1.x.

- https://github.com/beam-telemetry/telemetry_metrics_prometheus -- elixir
- https://github.com/deadtrickster/prometheus-cowboy#exporting-cowboy2-metrics prometheus.erl/cowboy
- https://elixirforum.com/t/elixir-blog-post-monitoring-hackney-connection-pools-with-telemetry/44640
- https://medium.com/the-realreal-product-tech-blog/monitoring-hackney-connection-pools-with-telemetry-3aaeafa8eeb8
- https://github.com/beam-telemetry/telemetry

Push OTel to a sidecar using gRPC or protobufs or whatever, and then have _that_ publish to Kafka or wherever?

Mind map:

- Can I use "observability" until I've played with honeycomb?
- Logging
  - Lager.
  - The new stuff.
  - Compare with Elixir's logger.
  - syslog/rsyslog.
  - 12 factor app (and K8s) just say to write to stdout, let other layer deal with it.
    - K8s log truncation.
    - What ways are there to ship the logs elsewhere?
  - Fluentd.
- things like Splunk, Scalyr, etc.
  - Twilio publishes to Bigtable -- great for queries, not so good for live. Expensive.
- Telemetry - probably tackle after metrics.
- Metrics
- collectd, statsd, datadog
- graphite/carbon/grafana
- folsom, exometer, custom (mecs).
- Erlang VM stats.
- Adding stats for other libraries: poolboy, etc. mecs has plugins for all that crap; we had to do something similar at Electric Imp.
- prometheus
  - prom has tags, graphite doesn't.
  - Erlang VM stats, again.
- OpenCensus vs. OpenTelemetry
- `telemetry` is a way to provide a ubiquitous interface for different libraries to emit telemetry. It's not related to OTel (until you want it to be).
  - if you've got folsom or exometer, how do you tell the library (cowboy, hackney, etc.) to use that metrics library?
    - https://github.com/benoitc/hackney#metrics
    - https://ninenines.eu/docs/en/cowboy/2.9/manual/cowboy_metrics_h/
    - cowboy_telemetry is a separate library; it wraps cowboy_metrics_h and converts the metrics into telemetry events. See https://github.com/beam-telemetry/cowboy_telemetry/blob/main/src/cowboy_telemetry_h.erl#L15.
      - wrapping it is an interesting choice; I'd probably have been more explicit.
      - how _do_ you use cowboy_metrics_h anyway?
        - See this https://github.com/deadtrickster/prometheus-cowboy#exporting-cowboy2-metrics
        - prometheus-cowboy does two (semi-related) things:
          1. a cowboy handler to export prometheus metrics from prometheus.erl.
          2. the above is how to export cowboy metrics from cowboy_metrics_h to prometheus.erl.
    - telemetry =/= metrics; cowboy reports telemetry now, so what's the difference?
    - https://medium.com/the-realreal-product-tech-blog/monitoring-hackney-connection-pools-with-telemetry-3aaeafa8eeb8
- if I'm pushing OTel (or is it pull?), can I publish metrics from _there_, rather than from my daemon?
- likewise: what OTel is there for host/node metrics?
- what OTel stuff is there for K8s? In terms of exporters, I mean.
- different metrics types
  - counter, gauge, histogram, etc.
- jaeger; what other options for that?
- alerting -- on metrics, on other?

{% include _series_toc.html %}


OK, so cowboy metrics:

- introduction to cowboy
- adding cowboy_metrics_h: `#{env => Env}, metrics_callback => fun handle_cowboy_metrics/1, stream_handlers => [cowboy_metrics_h, cowboy_stream_h]})`
- but how to report those metrics?
  - Logging? Good first step, but not that useful.
  - Publish them with folsom/exometer?
  - What about we publish them to prometheus?
    - ourselves?
    - using prometheus.erl?
      - in which case, use what it already provides.
- but we want OTel instead?
  - that's a thing as well. it wraps cowboy_metrics_h.
  - what if we want both? out of scope?
  - is there a `telemetry`-to-prometheus.erl connector?
    - isn't this getting a little bit circular?
  - do I need to draw some boxes?

But, again, compare these custom metrics vs. `telemetry` as a standard. Oh, and link to the Erlang Solutions blog post on this stuff.

Another comparison: hackney uses erlang-metrics, which is benoit's (?) attempt to make metrics callbacks generic.
Compare the metrics callbacks in ei_cache.

Should he have used telemetry? There's an open issue which talks about hooks, which would be kinda similar to cowboy's
handlers, and you could cook up your own telemetry handler.

Actually, why _does_ cowboy_telemetry_h wrap cowboy_metrics_h? You don't need to do that. We didn't do that at Electric
Imp, iirc -- we just wrote pre-/post- handlers. Maybe that doesn't work in Cowboy 2.x; we were on Cowboy 1.x.

- https://github.com/beam-telemetry/telemetry_metrics_prometheus -- elixir
- https://github.com/deadtrickster/prometheus-cowboy#exporting-cowboy2-metrics prometheus.erl/cowboy
- https://elixirforum.com/t/elixir-blog-post-monitoring-hackney-connection-pools-with-telemetry/44640
- https://medium.com/the-realreal-product-tech-blog/monitoring-hackney-connection-pools-with-telemetry-3aaeafa8eeb8
- https://github.com/beam-telemetry/telemetry

Other things that need tackling:

- How does the telemetry data get to the ... whatever?
  - If we're using Jaeger, for example, it's got a Kafka ingester; how do we push events to it?

So, basically, we've got `telemetry`, exposing events/metrics either to prometheus or logging.

hackney -> erlang-metrics -> folsom -> folsomite -> graphite -> grafana
hackney -> erlang-metrics -> exometer -> https://hexdocs.pm/prometheus_exometer/readme.html -> prometheus -> grafana

Is there a exometer -> graphite exporter? Yes: https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#exometer_report_graphite

Don't forget about InfluxDB or OpenTSDB.
- for example https://github.com/travelping/exometer_influxdb

There's also bolingbroke, my cheap-ass folsom viewer. I wonder whether we could do something with Livebook and Vegalite to pull prometheus-formatted metrics and render them nicely?

Other things to think about:
- wire formats: statsd/dogstatsd is simple text, even fits in UDP. Do I have an example server for this somewhere?
- push to OTel is Kafka (above) or gRPC, I think.
- Prometheus is text, but can be protobufs.

There's something to be said for a sidecar that pulls prometheus and pushes OTel, but -- obviously -- it'll only do metrics, not rich events.

Oh. Rich events. That's where we cross the streams between logging and telemetry again. Are we then looking at
JSON-formatted logging? Which is nice and rich, but a PITA to read.

Which protocols are push or pull? Graphite is push, prometheus is pull, Influx is push. Do some of them have modes where
the other direction also works?

Where to start?
