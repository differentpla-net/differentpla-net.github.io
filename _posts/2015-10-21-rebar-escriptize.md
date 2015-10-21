---
title: How does rebar escriptize work?
date: 2015-10-21 08:29+0000
---

## Hello World

You can create Erlang scripts quite simply. Just create a file something like the following:

    #!/usr/bin/env escript

    main(_Args) ->
        io:format("Hello World~n").

Then run it:

    escript hello

Or:

    chmod +x hello
    ./hello

## rebar escriptize

If you want to include multiple Erlang modules in your script, you can take
advantage of the fact that `escript` allows loading from an embedded ZIP
resource.

This is what the `rebar escriptize` command does. You can create a skeleton application using rebar:

**Note:** I'm using rebar 2 in these examples.

    # Create the skeleton application:
    rebar create-app appid=hello

    # Remove the boilerplate:
    rm src/hello_app.erl src/hello_sup.erl

    # Remove the {mod, {hello_app, ...}} line from the .app.src file:
    sed -i '/{mod.*}/d' src/hello.app.src

You need to create an entry point in `src/hello.erl`:

    -module(hello).
    -export([main/1]).

    main(_Args) ->
        io:format(hello_greetings:world()).

For demonstration purposes, I've created another module:

    -module(hello_greetings).
    -export([world/0]).

    world() ->
        "Hello World!~n".

Compile it using `rebar compile escriptize`; run it with `./hello`.

## Dependencies

The useful part about using `rebar` is that it can manage dependencies for you.
For example, if we needed to load some JSON, we might want to use *mochijson2*.
To do this, create `rebar.config`:

    {deps,
     [
      {mochijson2, ".*",
       {git, "https://github.com/rlipscombe/mochijson2.git"}}
     ]}.

    {escript_incl_apps, [mochijson2]}.

Update `hello.erl` to use a new module:

    -module(hello).
    -export([main/1]).

    main([Path] = _Args) ->
        io:format("~s~n", [hello_json:from_file(Path)]).

Create the `hello_json.erl` file:

    -module(hello_json).
    -export([from_file/1]).

    from_file(Path) ->
        {ok, JSON} = file:read_file(Path),
        {struct, Props} = mochijson2:decode(JSON),
        proplists:get_value(<<"greeting">>, Props).
    
And, to save typing, a Makefile:

    all:
        rebar get-deps compile escriptize

Create a sample `world.json` file:

    {"greeting": "Hello World!"}

Compile and run it:

    make && ./hello world.json

## How does that work, then?

The `hello` script actually looks like this:

    #!/usr/bin/env escript
    %%
    %%! -pa hello/hello/ebin
    PK...

...where `PK` is the ZIP file magic number. That is: it's got a shebang header,
some escript directives and then everything else is attached as a ZIP file.
What's in the ZIP file?

    $ unzip -l hello
    Archive:  hello
    warning [hello]:  51 extra bytes at beginning or within zipfile
      (attempting to process anyway)
      Length      Date    Time    Name
    ---------  ---------- -----   ----
            0  2015-10-21 10:13   hello/
            0  2015-10-21 10:13   hello/ebin/
          240  2015-10-21 10:13   hello/ebin/hello.app
          944  2015-10-21 10:13   hello/ebin/hello.beam
          812  2015-10-21 10:13   hello/ebin/hello_greetings.beam
         1144  2015-10-21 10:13   hello/ebin/hello_json.beam
            0  2015-10-21 10:13   mochijson2/
            0  2015-10-21 10:13   mochijson2/ebin/
          234  2015-10-21 10:13   mochijson2/ebin/mochijson2.app
        43544  2015-10-21 10:13   mochijson2/ebin/mochijson2.beam
        16100  2015-10-21 10:13   mochijson2/ebin/mochinum.beam
    ---------                     -------
        63018                     11 files

## We can do that ourselves!

You don't need `rebar escriptize` to do this; it's quite simple to write an
`escriptize` script. See https://github.com/rlipscombe/escriptize for example.
