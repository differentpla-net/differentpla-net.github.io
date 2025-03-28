## Using KUBECONFIG

The K8s config file allows for multiple contexts, so we need a way to specify the context. The "default" context is
usually fine, though.

The default file is `~/.kube/config`.

> By default, kubectl looks for a file named config in the $HOME/.kube directory. You can specify other kubeconfig files by setting the KUBECONFIG environment variable

-- https://kubernetes.io/docs/concepts/configuration/organize-cluster-access-kubeconfig/

KUBECONFIG can be colon- (or semicolon-, for Windows) -delimited. The files are merged. Annoying.

We first need to get `current-context`. Actually, we should probably verify the version, etc., first.

So:

```erlang
% KUBECONFIG can specify multiple config files; we don't support that yet. We also only support Unix.
EnvSeparator = case os:type() of {unix, _} -> ":" end,
[KubeConfigPath] = string:split(os:getenv("KUBECONFIG"), EnvSeparator).

% There can be multiple documents in a YAML file (separated by ---); we don't support that yet.
[KubeConfig] = yamerl:decode_file(KubeConfigPath).

% Verify that this is actually a Kubernetes configuration file.
"Config" = proplists:get_value("kind", KubeConfig).
"v1" = proplists:get_value("apiVersion", KubeConfig).

% Get the current context name. Note that there's no default value for this.
CurrentContextName = proplists:get_value("current-context", KubeConfig).

% Get the current context.
{value, CurrentNamedContext} = lists:search(
    fun(Context) ->
        Name = proplists:get_value("name", Context),
        Name =:= CurrentContextName
    end,
    Contexts).

% That returns a named context; we want the nested context.
CurrentContext = proplists:get_value("context", CurrentNamedContext).

% From the current context, get the cluster name.
CurrentClusterName = proplists:get_value("cluster", CurrentContext).

% From the clusters, get the named cluster.
Clusters = proplists:get_value("clusters", KubeConfig).
{value, CurrentNamedCluster} = lists:search(fun(Cluster) -> Name = proplists:get_value("name", Cluster), Name =:= CurrentClusterName end, Clusters).

CurrentCluster = proplists:get_value("cluster", CurrentNamedCluster).

% Get the server name.
ServerName = proplists:get_value("server", CurrentCluster).

% Also get the SNI server name. We'll use the server name as the default.
TlsServerName = proplists:get_value("tls-server-name", CurrentCluster).
```

## Getting the user credentials

```erlang

```

## Making the request

Given all of that, we should be able to list (e.g.) nodes:

```erlang
Url = hackney_url:make_url(ServerName, <<"/api/v1/nodes">>, [{limit, 500}]).
Headers = [{<<"Accept">>, <<"application/json">>}].
SslOptions = [{server_name_indication, TlsServerName}].
Options = [{ssl, SslOptions}].
hackney:request(get, Url, Headers, <<>>, Options).
```