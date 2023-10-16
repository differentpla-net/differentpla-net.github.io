---
title: "httpc:request hangs with an HTTPS URL"
date: 2023-10-16T13:53:00.000Z
tags: erlang
---

If you use Erlang/OTP's provided HTTP client as follows, it hangs:

{% raw %}
```erlang
{ok, _} = application:ensure_all_started(inets),
{ok, {{_, 200, _}, Headers, Body}} =
    httpc:request(get, {"https://www.erlang.org", []}, [{ssl, [{verify, verify_none}]}], []).
```
{% endraw %}

This is covered in the documentation, but it's not obvious:

> If the scheme `https` is used, the `SSL` application must be started.

...and it would be nicer if it didn't hang.

To fix it, change your code like this:

{% raw %}
```erlang
{ok, _} = application:ensure_all_started(inets),
{ok, _} = application:ensure_all_started(ssl),
{ok, {{_, 200, _}, Headers, Body}} =
    httpc:request(get, {"https://www.erlang.org", []}, [{ssl, [{verify, verify_none}]}], []).
```
{% endraw %}

If you're using OTP 26 or newer, `application:ensure_all_started/1` takes a list, so this also works:

```erlang
{ok, _} = application:ensure_all_started([inets, ssl]),
% etc.
```
