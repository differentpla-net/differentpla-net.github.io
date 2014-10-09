---
title: Reverting Erlang 17
date: 2014-10-09T12:22:00Z
---

We've found a couple of problems with Erlang R16 that mean that one of our
projects requires Erlang 17. Conversely, we've found a couple of problems with
Erlang 17 that mean that one of our *other* projects should stick with Erlang
R16.

In a [previous post](http://blog.differentpla.net/blog/2014/09/30/kerl-and-direnv/),
I explained how to use **kerl** to install multiple Erlang versions, and how to
use **direnv** to switch between them.

One of our *other* *other* projects doesn't work with **direnv**, and requires
that the system-default Erlang version is R16.

Fun, fun, fun.

This post will tell you how to revert an Ubuntu box from Erlang 17 to Erlang
R16.

The following command will remove any system-installed Erlang packages:

    sudo apt-get remove $(dpkg --get-selections | grep ^erlang | cut -f1)

The next bit varies, depending whether you're on Ubuntu 12.04 (in which case
you'll want to use the Erlang Solutions packages) or 14.04 (in which case, you
can use the Ubuntu packages, but might not want to).

## Ubuntu 12.04

I'll assume that you're using the Erlang Solutions packages to get version 17,
in which case you'll have a `/etc/apt/sources.list.d/erlang-solutions.list`
file that looks like this:

    deb http://packages.erlang-solutions.com/ubuntu precise contrib

For **12.04** you need to create a file called (e.g.) `erlang-packages` containing the
following:

    erlang=1:16.b.3-3 erlang-appmon=1:16.b.3-3 erlang-asn1=1:16.b.3-3
    erlang-base=1:16.b.3-3 erlang-common-test=1:16.b.3-3
    erlang-corba=1:16.b.3-3 erlang-crypto=1:16.b.3-3 erlang-debugger=1:16.b.3-3
    erlang-dev=1:16.b.3-3 erlang-dialyzer=1:16.b.3-3 erlang-diameter=1:16.b.3-3
    erlang-edoc=1:16.b.3-3 erlang-eldap=1:16.b.3-3 erlang-erl-docgen=1:16.b.3-3
    erlang-et=1:16.b.3-3 erlang-eunit=1:16.b.3-3 erlang-examples=1:16.b.3-3
    erlang-gs=1:16.b.3-3 erlang-ic=1:16.b.3-3 erlang-ic-java=1:16.b.3-3
    erlang-inets=1:16.b.3-3 erlang-inviso=1:16.b.3-3
    erlang-jinterface=1:16.b.3-3 erlang-megaco=1:16.b.3-3
    erlang-mnesia=1:16.b.3-3 erlang-mode=1:16.b.3-3 erlang-observer=1:16.b.3-3
    erlang-odbc=1:16.b.3-3 erlang-os-mon=1:16.b.3-3
    erlang-parsetools=1:16.b.3-3 erlang-percept=1:16.b.3-3
    erlang-pman=1:16.b.3-3 erlang-public-key=1:16.b.3-3
    erlang-reltool=1:16.b.3-3 erlang-runtime-tools=1:16.b.3-3
    erlang-snmp=1:16.b.3-3 erlang-src=1:16.b.3-3 erlang-ssh=1:16.b.3-3
    erlang-ssl=1:16.b.3-3 erlang-syntax-tools=1:16.b.3-3
    erlang-test-server=1:16.b.3-3 erlang-toolbar=1:16.b.3-3
    erlang-tools=1:16.b.3-3 erlang-tv=1:16.b.3-3 erlang-typer=1:16.b.3-3
    erlang-webtool=1:16.b.3-3 erlang-wx=1:16.b.3-3 erlang-xmerl=1:16.b.3-3

Then run the following command:

    sudo apt-get install `cat erlang-packages`

## Ubuntu 14.04

If you've used the Erlang Solutions packages, you'll probably have a
`/etc/apt/sources.list.d/erlang-solutions.list` file that looks like this:

    deb http://packages.erlang-solutions.com/ubuntu trusty contrib

Your next decision: use the Ubuntu-provided Erlang R16 packages, or the Erlang
Solutions-provided packages?

Note that the Ubuntu packages don't seem to include `erlang-wx`, which means
that `observer` won't work.

### Erlang Solutions Packages

For the **Erlang Solutions packages**, edit your
`/etc/apt/sources.list.d/erlang-solutions.list` file so that it mentions
`precise`, rather than `trusty`:

    deb http://packages.erlang-solutions.com/ubuntu precise contrib

Then create an `erlang-packages` file, as shown in the **12.04** section,
above.

Then run the following commands:

    sudo apt-get update
    sudo apt-get install `cat erlang-packages`

### Ubuntu Packages

For the **Ubuntu packages**, create a file called (e.g.) `erlang-packages`
containing the following:

    erlang=1:16.b.3-dfsg-1ubuntu2.1
    erlang-appmon=1:16.b.3-dfsg-1ubuntu2.1
    erlang-asn1=1:16.b.3-dfsg-1ubuntu2.1
    erlang-base=1:16.b.3-dfsg-1ubuntu2.1
    erlang-common-test=1:16.b.3-dfsg-1ubuntu2.1
    erlang-corba=1:16.b.3-dfsg-1ubuntu2.1
    erlang-crypto=1:16.b.3-dfsg-1ubuntu2.1
    erlang-debugger=1:16.b.3-dfsg-1ubuntu2.1
    erlang-dev=1:16.b.3-dfsg-1ubuntu2.1
    erlang-dialyzer=1:16.b.3-dfsg-1ubuntu2.1
    erlang-diameter=1:16.b.3-dfsg-1ubuntu2.1
    erlang-edoc=1:16.b.3-dfsg-1ubuntu2.1
    erlang-eldap=1:16.b.3-dfsg-1ubuntu2.1
    erlang-erl-docgen=1:16.b.3-dfsg-1ubuntu2.1
    erlang-et=1:16.b.3-dfsg-1ubuntu2.1
    erlang-eunit=1:16.b.3-dfsg-1ubuntu2.1
    erlang-examples=1:16.b.3-dfsg-1ubuntu2.1
    erlang-gs=1:16.b.3-dfsg-1ubuntu2.1
    erlang-ic=1:16.b.3-dfsg-1ubuntu2.1
    erlang-ic-java=1:16.b.3-dfsg-1ubuntu2.1
    erlang-inets=1:16.b.3-dfsg-1ubuntu2.1
    erlang-jinterface=1:16.b.3-dfsg-1ubuntu2.1
    erlang-megaco=1:16.b.3-dfsg-1ubuntu2.1
    erlang-mnesia=1:16.b.3-dfsg-1ubuntu2.1
    erlang-mode=1:16.b.3-dfsg-1ubuntu2.1
    erlang-observer=1:16.b.3-dfsg-1ubuntu2.1
    erlang-odbc=1:16.b.3-dfsg-1ubuntu2.1
    erlang-os-mon=1:16.b.3-dfsg-1ubuntu2.1
    erlang-parsetools=1:16.b.3-dfsg-1ubuntu2.1
    erlang-percept=1:16.b.3-dfsg-1ubuntu2.1
    erlang-pman=1:16.b.3-dfsg-1ubuntu2.1
    erlang-public-key=1:16.b.3-dfsg-1ubuntu2.1
    erlang-reltool=1:16.b.3-dfsg-1ubuntu2.1
    erlang-runtime-tools=1:16.b.3-dfsg-1ubuntu2.1
    erlang-snmp=1:16.b.3-dfsg-1ubuntu2.1
    erlang-src=1:16.b.3-dfsg-1ubuntu2.1
    erlang-ssh=1:16.b.3-dfsg-1ubuntu2.1
    erlang-ssl=1:16.b.3-dfsg-1ubuntu2.1
    erlang-syntax-tools=1:16.b.3-dfsg-1ubuntu2.1
    erlang-test-server=1:16.b.3-dfsg-1ubuntu2.1
    erlang-toolbar=1:16.b.3-dfsg-1ubuntu2.1
    erlang-tools=1:16.b.3-dfsg-1ubuntu2.1
    erlang-tv=1:16.b.3-dfsg-1ubuntu2.1
    erlang-typer=1:16.b.3-dfsg-1ubuntu2.1
    erlang-webtool=1:16.b.3-dfsg-1ubuntu2.1
    erlang-xmerl=1:16.b.3-dfsg-1ubuntu2.1

Then run the following command:

    sudo apt-get install `cat erlang-packages`

## Did that work?

    $ erl
    Erlang R16B03-1 (erts-5.10.4) [source] [64-bit] [smp:12:12] [async-threads:10] [kernel-poll:false]

    Eshell V5.10.4  (abort with ^G)
    1> 

...seems to have done.

