---
title: Creating ZIP files in Erlang
date: 2014-03-05T16:30:57
tags: erlang
---
It's pretty simple to create a ZIP file on disk.

    Name = "foo.zip".
    {ok, FileName} = zip:create(Name, ["foo.txt"]).

What if you want to create a ZIP file completely in memory? Also pretty simple:

    Files = [{"foo.txt", <<"Contents of foo">>},
             {"bar.txt", <<"Contents of bar">>}].
    {ok, {"mem", ZipBin}} = zip:create("mem", Files, [memory]).

Then, if you want to write _that_ to a file:

    file:write_file("foobar.zip", ZipBin).

Why would you want to do this? Well, you might be creating an escript:

    escript:create('binary', [shebang, {archive, ZipBin}]).

Note that you need a module named for the script that exports a `main`
function, as follows:

    -export([main/1]).
    main(Args) ->
        % Whatever.
        ok.

To clarify that last bit: if your script is named "awesome", escript will look
for an entry point named `awesome:main/1`.

This has the useful side-effect that you can copy (or symlink) the script to
different names and use different entry points.
