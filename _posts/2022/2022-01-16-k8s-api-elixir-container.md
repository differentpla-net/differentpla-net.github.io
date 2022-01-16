---
title: "Accessing the Kubernetes API from an Elixir container"
date: 2022-01-16T13:04:00Z
tags: elixir kubernetes
---

If you want to access the Kubernetes API from Elixir, you should probably just use the
[k8s package](https://hex.pm/packages/k8s), but here's how to do it without taking that dependency.

From the inside of a pod, you can access the Kubernetes API by making HTTP requests to `kubernetes.default`. See [this post]({% post_url 2022/2022-01-07-k8s-api-in-container %}) for example. Here's how to do that from Elixir.

The Kubernetes API is available at `kubernetes.default`, or via some environment variables:

```elixir
# Or we can just use "https://kubernetes.default", which is guaranteed to be available.
host = System.fetch_env!("KUBERNETES_SERVICE_HOST")
port = System.fetch_env!("KUBERNETES_SERVICE_PORT") |> String.to_integer()
```

To make a query, you'll need the namespace. You can get your container's namespace as follows:

```elixir
namespace = File.read!("/var/run/secrets/kubernetes.io/serviceaccount/namespace")
```

You'll also need the path. The following lists pods with a particular label:

```elixir
labelSelector = "app.kubernetes.io/name=cluster-demo" # or whatever
path = "/api/v1/namespaces/#{namespace}/pods" # list pods
query = "labelSelector=#{labelSelector}"
```

You need to combine the various URI pieces together. Elixir's `URI` module doesn't seem to implement this, so you'll
need to either using string interpolation, or use Erlang's `uri_string` module. Here we do the latter:

```elixir
uri = :uri_string.recompose(%{scheme: "https", host: host, port: port, path: path, query: query})
```

In order to authenticate with the Kubernetes API, you'll need the service account token:

```elixir
token = File.read!("/var/run/secrets/kubernetes.io/serviceaccount/token")
```

Rather than take a dependency on an external HTTP client, we can juse use Erlang's built-in `httpc`:

```elixir
headers = [{'authorization', 'Bearer #{token}'}]
opts = [ssl: [verify: :verify_none], timeout: 15_000]

{% raw %}
{:ok, {{_, 200, _}, _, body }} = :httpc.request(:get, {uri, headers}, opts, [])
{% endraw %}
```

...and then you parse the response. Unfortunately at this point, you _do_ need to take a dependency on a JSON parser. We'll use [Jason](https://hex.pm/packages/jason).

```elixir
pod_list = Jason.decode!(body)
pod_ips = for item <- pod_list["items"], do: item["status"]["podIP"]
```

The above is the equivalent of the following:

```
$ kubectl --namespace cluster-demo \
    get pods -l app.kubernetes.io/name=cluster-demo -o json | \
    jq -r '.items[].status.podIP'    # using jq
10.42.3.93
10.42.4.82
10.42.2.99

$ kubectl --namespace cluster-demo \
    get pods -l app.kubernetes.io/name=cluster-demo \
             -o jsonpath='{range .items[*]}{.status.podIP}{"\n"}{end}' # using jsonpath
10.42.3.93
10.42.4.82
10.42.2.99
```
