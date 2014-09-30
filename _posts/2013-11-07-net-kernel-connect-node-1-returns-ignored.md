---
title: "net_kernel:connect_node/1 returns ignored"
date: 2013-11-07T16:31:34Z
---
Just some random thing I learnt today while playing with Erlang:

So, I was playing with connecting to another Erlang node; I'm thinking about how
to use it for diagnostics. However, when I tried it...

    1> net_kernel:connect_node('foo@roger-pc').
    ignored
    
Huh? The documentation says:

> Returns true if successful, false if not, and ignored if the local node is not alive.

Wait. What? Obviously the node is alive -- I'm using it...? Hang on, maybe Erlang
has a different meaning of "alive"?

Yeah:

    2> erlang:is_alive().
    false
    
Hmmm. Documentation again:

> Returns true if the local node is alive; that is, if the node can be part of a
> distributed system. Otherwise, it returns false.

Ah. I didn't give my local shell a name. Let's try again:

    $ erl -sname bar
    (bar@roger-pc)1> 
    
Well, that's more promising -- there's a name in the prompt...

    (bar@roger-pc)1> is_alive().
    true
    
Better.

    (bar@roger-pc)2> net_kernel:connect_node('foo@roger-pc').
    true
    
Perfect.
