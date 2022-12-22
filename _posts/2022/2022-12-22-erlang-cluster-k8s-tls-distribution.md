---
title: "Erlang cluster on Kubernetes: TLS distribution"
short_title: "TLS distribution"
date: 2022-12-22T09:52:00.000Z
layout: series
series: erlang-cluster-k8s
tags: erlang kubernetes
---

In the [previous post]({% post_url 2022/2022-12-21-erlang-cluster-k8s-erlang-clustering %}), we got clustering working
_without TLS_. Lifting from the investigation that I wrote up [here]({% post_url 2022/2022-11-12-erlang-tls-distribution %}),
I'll add TLS distribution to my Erlang cluster, but only with server certificates and with no verification (for now).

We need server certificates to allow encryption. We can't use verification because the certificates don't match the host
names.

## Enabling TLS distribution

To enable TLS distribution, we need to make some changes to `vm.args.src`:

```erlang
%...
-proto_dist inet_tls
-ssl_dist_optfile ${ROOTDIR}/inet_tls_dist.config
%...
```

This tells the Erlang runtime to use the `inet_tls_dist` module for distribution (the `_dist` is implicit), and provides
a file containing the various TLS options.

The `ROOTDIR` environment variable is set by the startup script; we use it to ensure a fully-qualified path to the
config file.

## Configuring TLS distribution

The configuration file looks like this:

```erlang
[
    {server, [
        {certfile, "/secrets/erlclu-dist-tls.crt"},
        {keyfile, "/secrets/erlclu-dist-tls.key"},
        {verify, verify_none},
        {secure_renegotiate, true}
    ]},
    {client, [
        {verify, verify_none},
        {secure_renegotiate, true}
    ]}
].
```

This configures the server's certificate and private key, and it disables peer verification for both the server and the
client. This allows us to get encryption (because of the keypair), but not authentication (because we're not actually
verifying the certificates).

The documentation says that the `certfile` must be a PEM file containing both the certificate and key. This isn't
true; you can use `certfile` and `keyfile` to specify them separately.

## Creating a self-signed certificate

My ultimate goal is to use a _cert-manager_ `Issuer` object to manage a private CA for issuing mTLS certificates. For,
now, however, we'll make do with a self-signed certificate. I'll use my [elixir-certs]({% post_url
2021/2021-12-21-elixir-certs %}) script for this:

```bash
./certs self-signed \
    --out-cert erlclu-dist-tls.crt --out-key erlclu-dist-tls.key \
    --template server \
    --subject "/CN=inet_tls_dist"
```

## Kubernetes TLS secret

We need to put the certificate in a secret:

```bash
kubectl --namespace erlclu \
    create secret tls erlclu-dist-tls \
        --cert=erlclu-dist-tls.crt \
        --key=erlclu-dist-tls.key
```

## Using the secret

...and we need to mount the secret where the pod is expecting it:

```yaml
#...
spec:
  containers:
  - name: erlclu
    #...
    volumeMounts:
      - name: erlclu-dist-tls
        mountPath: /secrets
  volumes:
    - name: erlclu-dist-tls
      secret:
        secretName: erlclu-dist-tls
        items:
          - key: tls.key
            path: erlclu-dist-tls.key
          - key: tls.crt
            path: erlclu-dist-tls.crt
#...
```

## Is it working?

Well, according to the home page, all of the nodes are talking to each other.

Is their communication encrypted? It's hard to test. I'll write two posts later to discuss that. One will be a complete
hack; the other will use Wireshark.

## Broken remote console

This does introduce one problem, however: `kubectl exec ... bin/erlclu remote_console` command now longer works. The
relx-generated script uses `erl_call` but `erl_call` knows nothing about TLS.

I asked on [the Erlang forums](https://erlangforums.com/t/using-myapp-remote-console-with-tls-distribution/2052), but in the meantime, I came up with my own workaround:

In `vm.args.src`, put the two new arguments on the _same_ line:

```
-proto_dist inet_tls -ssl_dist_optfile ${ROOTDIR}/inet_tls_dist.config
```

Because the startup script is looking for `/^-proto_dist/`, it picks up both arguments. This doesn't seem to break the
default Erlang runtime parsing.

Then tell it to use `nodetool`:

```
kubectl --namespace erlclu exec -it deploy/erlclu -- env "USE_NODETOOL=1" /erlclu/bin/erlclu remote_console
```

This works, but it takes several seconds to connect. I can think of two potential solutions:

1. Expose the remote console over SSH. This is the one I ended up choosing, because it's what we do at work. It was
   top-of-mind, and I wanted to know more about how it works.
2. Write a custom distribution protocol that doesn't require TLS for localhost connections. CouchDB, as far as I can
   tell, does this.

The second option is perhaps more user-friendly, because it works the same as before, and is thus less surprising. I
might spend some time looking at that in future.
