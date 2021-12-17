---
title: Cowboy - Request Timeouts
date: 2013-11-03T19:56:38Z
---
Cowboy protects itself against slow requests by doing the following:

At the top of `cowboy_protocol:init`, it works out, in clock time, when the
request should be finished:

    wait_request(#state{until=until(Timeout)}).

    until(infinity) -> infinity;
    until(Timeout) -> to_milliseconds(os:timestamp()) + Timeout.

By wrapping `Transport::recv` (which might be called multiple times while
parsing an incoming request) in order to track total time elapsed:

    recv(Socket, Transport, infinity) ->
        Transport:recv(Socket, 0, infinity);
    recv(Socket, Transport, Until) ->
        Now = to_milliseconds(os:timestamp())
        Timeout = Until - Now,
        if Timeout < 0 -> {error, timeout};
           true -> Transport:recv(Socket, 0, Timeout)
        end.
