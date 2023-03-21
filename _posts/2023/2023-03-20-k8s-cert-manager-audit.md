---
title: "Cert-Manager Audit"
date: 2023-03-20T13:15:00Z
tags: kubernetes cert-manager elixir livebook
---

In [Expired Certificates: Incident Review]({% post_url 2023/2023-03-19-expired-certificates-incident-review %}), I
listed a future action: _"Audit the cluster to see if there are any other TLS secrets that aren't using cert-manager."_
Here's how I did it using Elixir Livebook.

## Cert-manager metadata

A secret that's been issued by cert-manager has various annotations. Here's my `gitea-tls` secret (ellided, obviously):

```
% kubectl --namespace gitea get secret gitea-tls -o yaml
```

```yaml
apiVersion: v1
data:
  ca.crt: ...
  tls.crt: ...
  tls.key: ...
kind: Secret
metadata:
  annotations:
    cert-manager.io/alt-names: git.k3s.differentpla.net
    cert-manager.io/certificate-name: git-k3s-differentpla-net
    cert-manager.io/common-name: ""
    cert-manager.io/ip-sans: ""
    cert-manager.io/issuer-group: ""
    cert-manager.io/issuer-kind: ClusterIssuer
    cert-manager.io/issuer-name: k3s-ca-cluster-issuer
    cert-manager.io/uri-sans: ""
  creationTimestamp: "2023-03-18T20:06:04Z"
  name: gitea-tls
  namespace: gitea
  resourceVersion: "25878364"
  uid: f51be88d-503e-46ff-8133-48be6ba09f6b
type: kubernetes.io/tls
```

So that's how I'll do it:

1. Get all the secrets with `type: kubernetes.io/tls`.
2. Ignore the ones with cert-manager annotations.

If all of the TLS secrets are managed by cert-manager, the resulting list should be empty, right?

Well, no, there are other system-issued TLS secrets, but we can also ignore those.

## Install dependencies

```elixir
Mix.install([{:k8s, "~> 1.2"}])
```

## Connect

```elixir
{:ok, conn} = K8s.Conn.from_service_account()
:ok
```

Note that my Livebook installation has [cluster admin access]({% post_url 2022/2022-03-09-livebook-k8s-cluster-admin %}).

## List TLS secrets

```elixir
op = K8s.Client.list("v1", "Secret")
{:ok, secrets} = K8s.Client.run(conn, op)

tls_secrets =
  secrets["items"]
  |> Enum.filter(fn
    %{"type" => "kubernetes.io/tls"} -> true
    _ -> false
  end)
```

## Filter functions

```elixir
issued_by_cert_manager? = fn
  %{"metadata" => %{"annotations" => %{"cert-manager.io/issuer-name" => _}}} -> true
  _ -> false
end

is_in_namespace? = fn %{"metadata" => %{"namespace" => namespace}}, n -> namespace == n end
is_named? = fn %{"metadata" => %{"name" => name}}, n -> name == n end
```

## Run the query

```elixir
tls_secrets
|> Enum.reject(fn secret ->
  is_in_namespace?.(secret, "kube-system") or is_in_namespace?.(secret, "cert-manager") or
    is_in_namespace?.(secret, "longhorn-system")
end)
|> Enum.reject(&is_named?.(&1, "erlclu-ca-key-pair"))
|> Enum.reject(issued_by_cert_manager?)
```

We ignore system-issued TLS secrets, those in the cert-manager namespace itself, and also those managed by Longhorn. The
CA keypair for my [Erlang cluster]({% post_url 2022/2022-12-21-erlang-cluster-k8s-intro %}) is also manually-managed.

That first `Enum.reject` could probably be shortened by using `Enum.any?/2`, but I'm suffering from a cold at the
moment, so my brain's not really working properly.

The query should return an empty list.
