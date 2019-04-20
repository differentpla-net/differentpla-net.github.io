---
title: Debugging Erlang with a remote shell
date: 2014-05-08T15:54:00Z
tags: erlang
---
If you're connected to an Erlang node via a remote shell, and you don't have
access to stdout on the original node, you'll need to redirect the trace output
to your current shell.

Now, ordinarily, you'd just do:

    1> dbg:start().
    ** exception error: undefined function dbg:start/0

Oh. That's not good.

This is because you've got an Erlang release and it doesn't include the
`runtime_tools` library. Assuming that you've got a full-blown Erlang
installation on the box somewhere, you can do the following:

    2> code:add_pathz("/usr/lib/erlang/lib/runtime_tools-1.8.13/ebin").
    true

Obviously, the exact path to the `runtime_tools` library might vary on your
box.

    3> l(dbg).
    {module,dbg}
    4> dbg:start().
    {ok,<0.21511.0>}

Then you can start debugging:

    5> dbg:start().
    {ok,<0.21511.0>}
    6> dbg:tracer(process, {fun(Msg, _) -> io:format("~p\n", [Msg]) end, []}).
    {ok,<0.21511.0>}

This sets up the tracer so that it calls `io:format`, which (by default) will
send the output to your remote shell.

Then you can start adding traces:

    7> dbg:tpl(my_module, some_function, '_', []).
    {ok,[{matched,mynode@myhost,1}]}

If you want the return values:

    8> dbg:tpl(another_module, some_other_function, '_', [{'_', [], [{return_trace}]}]).
    {ok,[{matched,mynode@myhost,1},{saved,1}]}

Note that you can use `dbg:fun2ms/1` to make that slightly more readable:

    8> dbg:tpl(another_module, some_other_function, '_', dbg:fun2ms(fun(_) -> return_trace() end)).
    {ok,[{matched,mynode@myhost,1},{saved,1}]}

Make sure you tell it which processes you're interested in:

    9> dbg:p(pid(0,891,0), c).

Or:

    10> dbg:p(all, c).

Do your thing; enjoy the tracing.

Then turn it off:

    11> dbg:stop_clear().

