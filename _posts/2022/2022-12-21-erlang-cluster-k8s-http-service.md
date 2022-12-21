---
title: "Erlang cluster on Kubernetes: HTTP Service"
short_title: "HTTP Service"
date: 2022-12-21T19:47:00.000Z
layout: series
series: erlang-cluster-k8s
tags: erlang kubernetes
---

Because this is going to be a cluster of Erlang nodes, there's (obviously) going to be more than one instance. It makes
sense to add some kind of way to have some kind of "whoami" page, so that we can clearly see which node we're talking
to.

## Cowboy

To do this, I added a simple [cowboy](https://github.com/ninenines/cowboy) server to the `erlclu` release. I added the
dependency to `rebar.config` and added `cowboy` to the `applications` list in `erlclu.app.src`.

Then I added a simple router to `erlclu_app.erl`:

```erlang
start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", home_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(erlclu_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    erlclu_sup:start_link().
```

`home_handler` looks like this:

```erlang
-module(home_handler).

-export([init/2]).

init(Req0, Opts) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    Body = io_lib:format("~p~n", [node()]),
    Req = cowboy_req:reply(200, Headers, Body, Req0),
    {ok, Req, Opts}.
```

It outputs the Erlang node name. I'll add more information later.

## Service/Ingress

Then I added a Service and an Ingress. The service is fairly ordinary:

```yaml
apiVersion: v1
kind: Service
metadata:
  name: erlclu
  namespace: erlclu
spec:
  selector:
    app: erlclu
  type: ClusterIP
  ports:
  - name: http
    port: 8080
    targetPort: 8080
```

The ingress (I'm using Traefik) defines an HTTPS ingress that refers to the above service. It uses _cert-manager_ to
provision the certificate.

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress

metadata:
  name: erlclu
  namespace: erlclu
  annotations:
    # Traefik: secure, plz.
    traefik.ingress.kubernetes.io/router.entrypoints: websecure
    traefik.ingress.kubernetes.io/router.tls: "true"
    # Ask cert-manager to issue a TLS certificate.
    cert-manager.io/cluster-issuer: k3s-ca-cluster-issuer

spec:
  tls:
  - hosts:
      - erlclu.k3s.differentpla.net
    secretName: erlclu-http-tls
  rules:
  - host: erlclu.k3s.differentpla.net
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: erlclu
            port:
              name: http
```

I also updated the deployment to start 5 replicas:

```yaml
...
spec:
  replicas: 5
...
```

I added the host to my [custom CoreDNS server]({% post_url 2022/2022-02-25-coredns-custom %}). By navigating to
`erlclu.k3s.differentpla.net` and hitting Refresh, I can see that it's hitting the 5 different nodes. They're not
clustered yet. I'll fix that shortly.
