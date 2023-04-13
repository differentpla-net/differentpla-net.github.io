---
title: "Erlang on Kubernetes: Using a ConfigMap for sys.config"
date: 2023-04-13T16:27:00.000Z
tags: erlang kubernetes
---

Erlang configuration is typically stored in the `sys.config` file. What options do we have if we want to have different
settings in this file when deploying using Kubernetes?

## ConfigMap

One answer is to use a Kubernetes `ConfigMap` object, which you might do as follows:

```
apiVersion: v1
kind: ConfigMap
metadata:
  name: myapp-config
  namespace: myapp
data:
  sys.config: |
    [
      {myapp, []},

      {kernel, [
        {logger_level, info},
        {logger, [{handler, default, logger_std_h, #{formatter => {logger_formatter, #{}}}}]}
      ]}
    ].
```

Then you can mount the ConfigMap as follows:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: myapp
  namespace: myapp
spec:
  # ...
  template:
    spec:
      containers:
        - name: myapp
          # ...
          env:
            - name: RELX_CONFIG_PATH
              value: /myapp/config/sys.config
          volumeMounts:
            - name: myapp-config
              mountPath: /myapp/config
      volumes:
        - name: myapp-config
          configMap:
            name: myapp-config
```

By default, Erlang releases look for the `sys.config` file in the `.../releases/$VSN/` directory, which -- as discussed
[earlier]({% post_url 2022/2022-12-22-erlang-versioning %}) -- is awkward to find. We take advantage of the
`RELX_CONFIG_PATH` environment variable to override this default.

## Generated config files

Relx allows you to generate the `sys.config` and `vm.args` files at runtime. To do this, include `config/sys.config.src`
and `config/vm.args.src` files in your project, respectively. You're more likely to do this with `vm.args`, so that's
what the following example shows.

But if you try to do this with a ConfigMap, as follows (and assuming you've set the `VMARGS_PATH` environment variable),
it will fail.

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: myapp-config
  namespace: myapp
data:
  vm.args.src: |
    -name myapp@${MY_POD_IP}
    -setcookie ${RELEASE_COOKIE}
```

Because ConfigMap volumes are mounted read-only, the relx startup script will be unable to convert `vm.args.src` into
`vm.args` and will fail.

## Copying from the ConfigMap

To solve this, we need some way to copy the files from the (read-only) ConfigMap volume to the somewhere writeable.
There are a number of options.

### A relx pre-start hook

Relx allows you to run scripts before it starts your Erlang release, using "hooks". See [the relx
documentation](https://github.com/erlware/relx/wiki/Configuration#start-script-hooks).

Unfortunately, **this doesn't work**; relx attempts to generate the configuration files before it runs the hooks.

### An init container

Another option would be to include an init container in the deployment that mounts the ConfigMap and copies the files to
another volume that's shared with the main container. This is workable, but has a bunch of moving parts, making it more
complex.

### Wrap the startup script

This is currently my preferred option. For this, we change the `Dockerfile` as follows:

```Dockerfile
ENTRYPOINT ["/sbin/tini", "--"]
CMD ["/entrypoint.sh", "/myapp/bin/myapp", "foreground"]
```

The `entrypoint.sh` file can look like this:

```sh
#!/bin/sh

set -eu

cp -r "${CONFIG_SOURCE}" "${CONFIG_TARGET}"
exec "$@"
```

We need to set those environment variables:

```yaml
          # ...
          env:
            - name: CONFIG_SOURCE
              value: /myapp/_config   # note the underscore
            - name: CONFIG_TARGET
              value: /myapp/config
            - name: VMARGS_PATH
              value: /myapp/config/vm.args
            - name: RELX_CONFIG_PATH
              value: /myapp/config/sys.config
          volumeMounts:
            - name: myapp-config
              mountPath: /myapp/_config
```

And then our startup script copies the files from the ConfigMap volume to somewhere writable, and we're good.

Incidentally, for a more flexible way to wrap the startup, see [this
article](https://www.camptocamp.com/en/actualite/flexible-docker-entrypoints-scripts/).

Of course, if you're going to use an init container or startup script, you might want to use your own config generation
instead of that provided by relx.

## Related Links

- <http://lrascao.github.io/k8s-erlang-configuration/#erlang-vm-configuration> -- uses cuttlefish and a pre-start hook.
  I'm not quite sure how it works; I suspect it has a placeholder config file to keep the startup script happy before
  running the hook.
- <https://adoptingerlang.org/docs/production/kubernetes/#container-environments-and-configmaps> -- uses environment
  variables from the ConfigMap, and relies on relx to do the substitutions.
