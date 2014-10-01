# Displaying git revision in an Erlang application

## Where's it stored?

The version number is in your `ebin/foo.app` file, which is (if you're using
rebar) generated from the `{vsn, Vsn}` tuple in your `src/foo.app.src` file.

## Querying the version

In Erlang, you get the revision number by using:

    {ok, Vsn} = application:get_key(foo, vsn).

## Version requirements

Erlang/OTP merely
[defines](http://www.erlang.org/doc/design_principles/applications.html#id73727)
`Vsn` as being "a string".

I guess this means you can use whatever you want. I'm [not
sure](http://stackoverflow.com/q/20969425/8446) if upgrades require some kind
of ordering to the values.

## Updating the version using rebar

If you put `{vsn, git}` in your `src/foo.app.src` file, rebar will replace it
with [the
output](https://github.com/rebar/rebar/blob/master/src/rebar_utils.erl#L476) of
`git describe --always --tags` when generating the `ebin/foo.app` file.

It'll look like "v0.3" if your checked out code represents a tag, or (e.g.)
"v0.3-1-g253d91a" if you're ahead of the tag.

It's fairly cool, but it has some limitations:

 * If you're at a tag, it puts (e.g.) "v0.3", and omits the SHA.
 * It doesn't pay attention to local, uncommitted modifications, so you get
   "v0.3" even if you're compiling with local modifications.

## Updating the version using `rebar_vsn_plugin`

Alternatively, you can use the
[rebar_vsn_plugin](https://github.com/erlware/rebar_vsn_plugin), which extends
the default behaviour.

I couldn't get it to do anything more than use just the version number, though.
YMMV.

## Updating the version using rebar and a custom command

Fortunately, rebar allows the command to be used to be replaced with something
custom, by using `{vsn, {cmd, "my_vcs_vsn_cmd"}}` in your `src/foo.app.src` file.

For example, using `{vsn, {cmd, "git rev-parse --short HEAD"}}` will always
give you the 7-character SHA value.

So, combined with the previous article (link TODO), we can do something intelligent.

