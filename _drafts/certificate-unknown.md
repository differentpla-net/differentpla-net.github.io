# Erlang SSL - "certificate unknown" error

I've been adding client certificate support to one of our servers, and I ran
into a problem when integration testing it.

I was getting `SSL: certify: ssl_handshake.erl:426:Fatal error: certificate
unknown`

Some preliminary investigation showed that it only happened when I was using
the `verify_fun` SSL option. I need this in the server to verify that the
client certificate meets certain requirements (for example, that it has the
correct CN).

Confusingly, this was only happening from my integration test code; the real
client was fine.

A quick look at the Erlang source code, and I was able to put together some
tracing in the shell:

    dbg:start().
    dbg:tracer().

    dbg:tpl(ssl_handshake, apply_user_fun, '_', []).

    dbg:p(all, c).

Then I ran some example client code from another Erlang shell:

    Port = 12345.
    Opts = [binary, {active, false}, {packet, raw},
            {certfile, "client.crt"}, {keyfile, "client.key"},
            {verify, verify_peer}, {cacertfile, "ca.crt"}].
    {ok, S} = ssl:connect("localhost", Port, Opts).

The first shell (the one with the tracing enabled) then printed something like
the following:

    call ssl_handshake:apply_user_fun(#Fun<foo.1.12345678>,{'OTPCertificate',...},
        {extension,{2,5,29,14},...},<<"expected-cn">>,server)

Note that `<<"expected-cn">>` is the `UserState` passed to the verify function.

Ah! I've not implemented all of the expected clauses in my verify function.

So, there are several things to learn here:

1. Integration tests are useful. Write some.
2. Erlang's SSL client seems to behave slightly differently from the one our real client uses.
3. Implement all of the clauses in your `verify_fun`. More below.


