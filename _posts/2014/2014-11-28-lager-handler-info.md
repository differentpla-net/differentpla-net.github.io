---
title: Getting lager handler info
date: 2014-11-28T11:08:49Z
tags: erlang lager
---

We use [lager](https://github.com/basho/lager) for our logging at [Electric Imp](http://www.electricimp.com).
This morning I had cause to [tweak the
configuration](http://blog.differentpla.net/blog/2014/05/10/adding-lager-handlers-at-runtime/)
at runtime on one of our staging boxes, but first I needed to figure out which
handlers were already installed.

**The following was done on a dev box, for demonstration purposes. The
configuration is slightly different in production.**

## which_handlers

Lager uses `gen_event` to distribute log events, so I started with:

{% highlight erlang %}
    (imp_server@roger-pc) 1> gen_event:which_handlers(lager_event).
    [lager_rabbitmq_backend,lager_folsom_backend,
     {lager_syslog_backend,{"imp_server",local7}},
     {lager_file_backend,"log/console.log"},
     {lager_file_backend,"log/error.log"},
     lager_console_backend,lager_backend_throttle]
{% endhighlight %}

Here you can see that we have several different backends registered:

 * One that talks to RabbitMQ; we use this one to email errors to the back-end
   team. If there's a spike in the number of error emails, someone is
   automatically paged to investigate. The emails are also regularly triaged to
   see if they signify a bug.
 * One that reports metrics to graphite (via folsom and folsomite).  Along with
   all of the other metrics gathered, we find it useful to track warning and
   error counts across each server.
 * One that reports to syslog. We use syslog to ship everything back to a
   central log server, making it easier to correlate events in one place. We're
   considering whether to replace this with logstash.
 * Two for writing to local log files.
 * The console backend.
 * The default `lager_backend_throttle` handler.

However, this doesn't tell us anything about the handler configuration. For
that, we have to dig a little deeper.

## lager_event state

One option is to use `sys:get_state(lager_event)`, which queries the state of
the `lager_event` event manager. In our example, it looks something like the
following:

    (imp_server@roger-pc) 2> sys:get_state(lager_event).
    [{lager_rabbitmq_backend,false,
                             {state,{mask,15},
                                    <0.119.0>,lager_rabbitmq_formatter,
                                    [{process,"imp_server"},{environment,"development"}]}},
     {lager_folsom_backend,false,{state,imp_server,{mask,31}}},

...etc.

This shows the *state* of the event handlers, which is not necessarily the same
as the *initial configuration parameters*. It's still useful, though.

## which_children

More useful, however, is the realisation that lager event handlers [are
supervised](http://blog.differentpla.net/blog/2014/11/07/erlang-sup-event/).
This means that the supervisor *must* have a copy of the original configuration
parameters -- otherwise it couldn't restart them properly.

Unfortunately, we can't get hold of that information directly. What we can do
is recognise that `lager_handler_watcher_sup` supervises
`lager_handler_watcher` processes, and those, too, must have the original
configuration somewhere:

We can get hold of the watchers:

    (imp_server@roger-pc) 3> Watchers = supervisor:which_children(lager_handler_watcher_sup).
    [{undefined,<0.80.0>,worker,[lager_handler_watcher]},
     {undefined,<0.82.0>,worker,[lager_handler_watcher]},
     {undefined,<0.84.0>,worker,[lager_handler_watcher]},
     {undefined,<0.101.0>,worker,[lager_handler_watcher]},
     {undefined,<0.122.0>,worker,[lager_handler_watcher]},
     {undefined,<0.91.0>,worker,[lager_handler_watcher]},
     {undefined,<0.76.0>,worker,[lager_handler_watcher]},
     {undefined,<0.78.0>,worker,[lager_handler_watcher]}]

And then we can inspect any one of those to get the original configuration:

    (imp_server@roger-pc) 4> sys:get_state(pid(0,91,0)).
    {state,lager_folsom_backend,
           [imp_server,warning],
           lager_event}

And there's the backend and configuration:
`lager_folsom_backend,[imp_server,warning]`. Note that this relies on the
internal state of `lager_handler_watcher`, which might change.

We can make this prettier by use of the following:

    [begin
        {state, Module, Config, Event} = sys:get_state(Pid),
        {Event, Module, Config}
     end || {_, Pid, _, _} <- supervisor:which_children(lager_handler_watcher_sup)].

This outputs something like the following (I've ellided some of the details):

    [{lager_event,{lager_file_backend,"log/error.log"}, ...},
     {lager_event,{lager_file_backend,"log/console.log"}, ...},
     {lager_event,{lager_syslog_backend,{"imp_server",local7}}, ...},
     {lager_event,lager_rabbitmq_backend, ...},
     {error_logger,error_logger_lager_h,[500]},
     {lager_event,lager_folsom_backend,[imp_server,warning]},
     {lager_event,lager_backend_throttle,[20,5]},
     {lager_event,lager_console_backend,
                  [info,{lager_imp_formatter,[]}]}]

Of interest here is that you can see all of our backends, with configuration,
but also `error_logger_lager_h`, which is responsible for turning
`error_logger` events into `lager_event` events; and also the configuration for
`lager_backend_throttle`, which controls when it starts exerting back pressure.

Awesome. We're done.

