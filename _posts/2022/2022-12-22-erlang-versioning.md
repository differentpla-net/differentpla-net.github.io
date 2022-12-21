---
title: "Erlang application versioning"
date: 2022-12-22T13:53:00.000Z
tags: erlang rebar3
---

When you're investigating a problem with a deployed application, it's useful to know precisely which version you're
looking at. Here's how to automatically set the version number in an Erlang release.

Erlang actually allows you to set the version number for your release, for each application within that release, and for
each module within an application. I'll write about how these are usually controlled, how to find out what they are at
runtime, and how to automatically set them.

I'm going to assume you're using [Rebar 3](https://rebar3.org/). All of the examples use `myapp` for the application
name.

## Release

### Setting the release version

The release version is set in the `relx` section of `rebar.config`:

```erlang
{relx, [
    {release, {myapp, "0.1.0"}, [
        myapp,
        %...
```

### Getting the release version

To discover the release version at runtime, use one of the following:

- If you're using the Rebar3-generated startup script, it's in the `RELEASE_VSN` environment variable.
- Call `release_handler:which_releases()`.
- Parse the `$ROOTDIR/releases/RELEASES` file. You can use `code:root_dir()` if you don't want to use the environment
  variable.

There may be other standard ways to do this, but I'm not aware of them.

In the past (at Electric Imp), we've used relx overlays to write a marker file containing the release version; and we've
created a `$ROOTDIR/releases/current` symlink in the generated tarball or container to point to the (e.g.) `0.1.0`
directory. Both of these make it easier for non-Erlang tooling to find the current release.

### Automatically setting the release version

The [Rebar3 documentation](https://rebar3.org/docs/deployment/releases/) gives a number of ways to do it, including from
git or by running a command.

I prefer setting it from an environment variable, because it gives you more control.

Rebar 3 doesn't automatically expand environment variables, so you can't just do this:

```erlang
{relx, [
    % This DOES NOT work:
    {release, {myapp, "$RELEASE_VSN"}, [
        myapp,
        %...
```

Instead, you need to use a command, as follows:

```erlang
{relx, [
    {release, {myapp, {cmd, "echo $RELEASE_VSN"} }, [
        myapp,
        %...
```

If the environment variable isn't set, you end up with an empty version number, which might be bad, so you'll need a
more complicated command to deal with that.

Alternatively, you could make use of `rebar.config.script`, which allows you to run arbitrary snippets of Erlang at
build time. To do this, you put a placeholder value (possibly empty) in `rebar.config`:

```erlang
{relx, [
    % Placeholder; see rebar.config.script.
    {release, {myapp, ""}, [
        myapp,
        %...
```

Then you need to create `rebar.config.script` as follows:

```erlang
case os:getenv("RELEASE_VSN") of
    false -> throw({unset_env_var, "RELEASE_VSN"});
    "" -> throw({empty_env_var, "RELEASE_VSN"});
    Vsn ->
        % Update release version
        {relx, Relx0} = lists:keyfind(relx, 1, CONFIG),
        {release, {Name, _Vsn}, Apps} = lists:keyfind(release, 1, Relx0),
        Relx = lists:keyreplace(release, 1, Relx0, {release, {Name, Vsn}, Apps}),
        lists:keyreplace(relx, 1, CONFIG, {relx, Relx})
end.
```

This is run as a single Erlang expression. The original `rebar.config` is available as the `CONFIG` value, and you're
expected to return a transformed configuration.

In the above, we use `lists:keyfind` and `lists:keyreplace` calls to find and replace the placeholder version value.

## Application

Each application within an Erlang release also has its own version number.

### Setting the application version

It's in the `src/myapp.app.src` file:

```erlang
{application, myapp, [
    {description, "This app is going to make me SO MUCH MONEY"},
    {vsn, "0.1.0"},
```

### Getting the application version

```erlang
{ok, Vsn} = application:get_key(myapp, vsn).
```

### Automatically setting the application version

As with the release version, rebar3 supports `git`, `cmd`, etc. for [setting the application
version](https://rebar3.org/docs/basic_usage/) at build time.

Again, I'm going to show how to use a script.

Create `src/myapp.app.src.script` containing the following:

```erlang
case os:getenv("RELEASE_VSN") of
    false -> throw({unset_env_var, "RELEASE_VSN"});
    "" -> throw({empty_env_var, "RELEASE_VSN"});
    Vsn ->
        {application, Name, App0} = lists:keyfind(application, 1, CONFIG),
        App = lists:keyreplace(vsn, 1, App0, {vsn, Vsn}),
        lists:keyreplace(application, 1, CONFIG, {application, Name, App})
end.
```

It's essentially the same as `rebar.config.script` above, so there's not a lot to discuss.

## Module versions

### Setting the module version

Erlang modules also have a version number. By default, it's just the MD5 hash of the module, but you can set it with the
`-vsn` [module attribute](https://www.erlang.org/doc/reference_manual/modules.html):

```erlang
-module(myapp).
-vsn("0.1.0").

%...
```

### Getting the module version

If the module is loaded, you can get it with `Mod:module_info(attributes)`. For example:

```erlang
Vsn = proplists:get_value(vsn, myapp:module_info(attributes)).
```

Or you can use `beam_lib:version/1`:

```erlang
{ok, {_Mod, Vsn}} = beam_lib:version(".../ebin/myapp.beam").
```

### Automatically setting the module version with `-D`

The value used in the `-vsn` module attribute can be a macro:

```erlang
-module(myapp).
-vsn(?VSN).

%...
```

To define the macro, you can use the compiler's `-D` option. If you're using rebar, this needs to be done in `erl_opts`:

```erlang
{erl_opts, [
    debug_info,
    {d, 'VSN', "0.1.0"}
]}.
```

To set this automatically, you'll need to update `rebar.config.script`. I'll leave that as an exercise for the reader.

### Setting the module version with a parse transform

Another option is to use a [parse transform](https://www.erlang.org/doc/man/erl_id_trans.html). I prefer this for two
reasons:

1. It doesn't require editing all of your source files.
2. I didn't think about the `-D` option until writing this blog post.

A "parse transform" is a custom module that's called by the compiler. It gets passed the abstract syntax tree for the
Erlang module that's being compiled and can make changes to it before it's compiled.

I wrote [vsn_transform](https://github.com/rlipscombe/vsn_transform/) to automatically add the `-vsn` module attribute
at compilation time. Similarly to rebar3, it can run a command, or take a literal value.

You add it to `rebar.config` as follows:

```erlang
{erl_opts, [
    debug_info,
    {parse_transform, vsn_transform},
    {vsn, "0.1.0"}
]}.

{deps, [
    %...
    {vsn_transform, {git, "https://github.com/rlipscombe/vsn_transform.git", {tag, "1.0.2"}}}
]}.
```

### Setting the module version with a parse transform, automatically

But we're all about scripting in this blog post, so we'll omit the `vsn` option from `rebar.config`:

```erlang
{erl_opts, [
    debug_info,
    {parse_transform, vsn_transform}
]}.

%...
```

Instead, we'll use `rebar.config.script` to set it:

```erlang
case os:getenv("RELEASE_VSN") of
    false -> throw({unset_env_var, "RELEASE_VSN"});
    "" -> throw({empty_env_var, "RELEASE_VSN"});
    Vsn ->
        % Update vsn_transform opts
        {erl_opts, Opts0} = lists:keyfind(erl_opts, 1, CONFIG),
        Opts = lists:keystore(vsn, 1, Opts0, {vsn, Vsn}),
        lists:keyreplace(erl_opts, 1, CONFIG, {erl_opts, Opts})
end.
```

Combining this with the earlier script, so that we can set the release version and module versions, we get the
following:

```erlang
case os:getenv("RELEASE_VSN") of
    false -> throw({unset_env_var, "RELEASE_VSN"});
    "" -> throw({empty_env_var, "RELEASE_VSN"});
    Vsn ->
        % Update vsn_transform opts
        {erl_opts, Opts0} = lists:keyfind(erl_opts, 1, CONFIG),
        Opts = lists:keystore(vsn, 1, Opts0, {vsn, Vsn}),
        CONFIG2 = lists:keyreplace(erl_opts, 1, CONFIG, {erl_opts, Opts}),

        % Update release version
        {relx, Relx0} = lists:keyfind(relx, 1, CONFIG2),
        {release, {Name, _Vsn}, Apps} = lists:keyfind(release, 1, Relx0),
        Relx = lists:keyreplace(release, 1, Relx0, {release, {Name, Vsn}, Apps}),
        lists:keyreplace(relx, 1, CONFIG2, {relx, Relx})
end.
```
