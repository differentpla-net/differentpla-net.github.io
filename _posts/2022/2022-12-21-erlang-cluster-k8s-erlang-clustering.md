---
title: "Erlang cluster on Kubernetes: Erlang Clustering"
short_title: "Erlang Clustering"
date: 2022-12-21T19:52:00.000Z
layout: series
series: erlang-cluster-k8s
tags: erlang kubernetes
---

Based on my [investigation with libcluster]({% post_url 2022/2022-01-08-libcluster-kubernetes %}) in Elixir, I've
decided to use DNS-based discovery for finding the other Erlang nodes in the cluster. To do this, we'll need a headless
service.

## Headless service

The best explanation of a headless service I've found is in [this YouTube video](https://www.youtube.com/watch?v=T4Z7visMM4E&t=778s). I'm going to mangle the explanation here.

Essentially: a ClusterIP Kubernetes Service object has its own IP address and acts as a load-balancing proxy for your
pods. A headless Service object doesn't (it's headless); instead it represents the IP addresses of the pods.

This is what we want: if we make a DNS request for a headless service, the response will contain the IP addresses of all
of the pods, rather than the IP address of the service object.

```yaml
# This file defines the headless service that allows DNS discovery to return all
# of the pods, rather than the service.
apiVersion: v1
kind: Service
metadata:
  name: erlclu-headless
  namespace: erlclu
spec:
  selector:
    app: erlclu
  # Explicitly setting clusterIP: "None" is what makes it a headless service.
  type: ClusterIP
  clusterIP: "None"
```

### dnsutils

We can explore this a little by using the `dnsutils` image:

```
% kubectl exec -i -t dnsutils -- /bin/bash
```

If we search for the "normal" service, we get the IP address of the service object:

```
root@dnsutils:/# dig +short erlclu.erlclu.svc.cluster.local
10.43.120.185
```

If we search for the headless service, we get the IP addresses of the relevant pods.

```
root@dnsutils:/# dig +short erlclu-headless.erlclu.svc.cluster.local
10.42.2.110
10.42.2.109
10.42.0.86
10.42.0.87
10.42.1.93
```

We're searching for `<service>.<namespace>.svc.cluster.local`. I'm not happy with the `erlclu` service name; I'll change
it to `erlclu-http` later.

### SRV queries

`libcluster` also implements a `DNSSRV` discovery strategy. This uses "SRV"-type DNS queries. They don't work unless you
define a `targetPort` on your headless service.

I didn't feel comfortable doing that because the Erlang clustering has nothing to do with the HTTP service, and I
couldn't decide an appropriate port number to use. I thought about using the epmd port (port 4369), but it's possible
that I might choose to go epmd-less in future, which would make that seem like an odd choice.

## Node naming

Erlang clustering requires that the client node and the server node agree on the server node's name. Because the DNS
response above has plain IP addresses in it, the server's node name needs a plain IP address in it.

We do this by using the Kubernetes [downward API](https://kubernetes.io/docs/concepts/workloads/pods/downward-api/) to
tell the pod its own IP address:

```yaml
env:
  # ...
  - name: MY_POD_IP
    valueFrom:
      fieldRef:
        fieldPath: status.podIP
```

Then we use that in `vm.args.src`:

```erlang
-name erlclu@${MY_POD_IP}
-setcookie ${RELEASE_COOKIE}
% ...
```

The node names will be `erlclu@10.42.0.86`, `erlclu@10.42.2.110`, etc.

## Automatic node discovery

To implement automatic node discovery, I added a `gen_server` module which periodically makes the appropriate DNS query and attempts to connect to the nodes it discovered:

```erlang
refresh() ->
    IPAddresses = inet_res:lookup("erlclu-headless.erlclu.svc.cluster.local", in, a),
    Nodes =
        [erlang:list_to_atom(
             lists:flatten(["erlclu@", inet:ntoa(A)]))
         || A <- IPAddresses],
    [net_kernel:connect_node(Node) || Node <- Nodes],
    ok.
```

That's it. It's not very complicated. I should probably make the `"erlclu-headless.erlclu.svc.cluster.local"` bit
configurable.

## Reporting nodes

It's tedious to access the remote console every time you want to check that clustering's working, so I added the
information to the HTTP endpoint. To make life easier, I also added `bbmustache` for simple templating.

```erlang
init(Req0, Opts) ->
    Priv = code:priv_dir(?APPLICATION),
    {ok, Template} =
        file:read_file(
            filename:join([Priv, "index.html"])),

    Headers = #{<<"content-type">> => <<"text/html">>},
    Body =
        bbmustache:render(Template,
                          #{node => node(),
                            nodes => lists:sort([node() | nodes()]),
                            cookie => erlang:get_cookie()},
                          [{key_type, atom}]),
    Req = cowboy_req:reply(200, Headers, Body, Req0),
    {ok, Req, Opts}.
```

```html
{% raw %}<body>
    <h1>{{ node }}</h1>
    <h2>Nodes</h2>
    <ul>
        {{#nodes}}
        <li>{{.}}</li>
        {{/nodes}}
    </ul>
    <h2>Cookie</h2>
    <pre>{{cookie}}</pre>
</body>{% endraw %}
```

Aside: I'm using Jekyll for this website; it uses Liquid templating. Mustache templates use a lot of the same markup.
Including mustache templates in Jekyll pages is annoying.

## Does it work?

Yes. We have successfully implemented basic discovery and clustering between Erlang nodes in a Kubernetes cluster. That's, like, about 20% of what I came here to do, however.

## Related posts

- [libcluster and Kubernetes]({% post_url 2022/2022-01-08-libcluster-kubernetes %})
- [Erlang clustering recap]({% post_url 2022/2022-11-11-erlang-clustering-recap %})
