---
title: "Erlang cluster on Kubernetes: Erlang Cookie"
short_title: "Erlang Cookie"
date: 2022-12-21T19:51:00.000Z
layout: series
series: erlang-cluster-k8s
tags: erlang kubernetes
---

I've covered this previously; see ["Erlang/Elixir Cookies and Kubernetes"]({% post_url 2022/2022-01-09-erlang-cookies-and-kubernetes %}). Here's the quick version.

First, we update the cowboy handler to report the Erlang cookie, so we can check that all of the nodes (remember:
they're behind an ingress; refreshing the browser will cycle through them) are using the same cookie.

```erlang
init(Req0, Opts) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    Body = io_lib:format("~p~n~p~n", [node(), erlang:get_cookie()]),
    Req = cowboy_req:reply(200, Headers, Body, Req0),
    {ok, Req, Opts}.
```

Then I updated the rebar-generated config files to pull the cookie from an environment variable:

```erlang
{relx, [
    %...
    {vm_args_src, "./config/vm.args.src"}
    %...
```

```erlang
% vm.args.src
-setcookie ${RELEASE_COOKIE}
```

Then I updated the deployment to set that environment variable from a secret:

```yaml
env:
- name: RELEASE_COOKIE # might need to be RELX_COOKIE for the remote_console to work.
  valueFrom:
    secretKeyRef:
      name: erlang-cookie
      key: cookie
```

Then I set the cookie:

```bash
ERLANG_COOKIE="$(env LC_CTYPE=C tr -dc 'A-Z' < /dev/random | head -c 20)"
kubectl --namespace erlclu \
    create secret generic erlang-cookie \
        --from-literal=cookie="$ERLANG_COOKIE"
```
