---
title: "Using k3s on Raspberry Pi: Docker Registry: What's in a name?"
date: 2021-12-10T12:27:00Z
layout: series
series: k3s
tags: k3s raspberry-pi
---

If we use our [private docker registry]({% post_url 2021-12-10-k3s-docker-registry %}) when specifying a container image, it fails:

```
  Warning  Failed     12s (x4 over 93s)  kubelet            Failed to pull image "10.43.236.176:5000/hello-world": rpc error: code = Unknown desc = failed to pull and unpack image "10.43.236.176:5000/hello-world:latest": failed to resolve reference "10.43.236.176:5000/hello-world:latest": failed to do request: Head "https://10.43.236.176:5000/v2/hello-world/manifests/latest": http: server gave HTTP response to HTTPS client
  Warning  Failed     12s (x4 over 93s)  kubelet            Error: ErrImagePull
```

This is because k8s (presumably `kubelet`) wants to use HTTPS when talking to the docker registry. This means
we need to either persuade it to use HTTP, like we did with the normal Docker engine, or we need to set up HTTPS.

For HTTPS, we need to generate a certificate, which needs to contain the server name. This is where we run into a problem: We're using ClusterIP networking, and the server name (which is an IP address) changes every time the pod is redeployed. Our image tags also refer to the registry name, and I'm not happy about that either.

From inside our containers, our service has a consistent name: `docker-registry.docker-registry.svc.cluster.local`, but that name doesn't mean anything outside a container. I wonder whether the container creator can deal with it? Let's try something:

```
$ kubectl --namespace docker-registry create deployment hello-world-2 \
    --image=docker-registry.docker-registry.svc.cluster.local:5000/hello-world
deployment.apps/hello-world-2 created

$ kubectl --namespace docker-registry get pods
NAME                               READY   STATUS             RESTARTS   AGE
hello-world-2-6d78bf744c-tjkp5     0/1     ImagePullBackOff   0          4m4s
...

$ kubectl --namespace docker-registry describe pod hello-world-2-6d78bf744c-tjkp5
...
Events:
  Type     Reason     Age                    From               Message
  ----     ------     ----                   ----               -------
  ...
  Warning  Failed     5m11s (x4 over 6m40s)  kubelet            Failed to pull image "docker-registry.docker-registry.svc.cluster.local:5000/hello-world": rpc error: code = Unknown desc = failed to pull and unpack image "docker-registry.docker-registry.svc.cluster.local:5000/hello-world:latest": failed to resolve reference "docker-registry.docker-registry.svc.cluster.local:5000/hello-world:latest": failed to do request: Head "https://docker-registry.docker-registry.svc.cluster.local:5000/v2/hello-world/manifests/latest": dial tcp: lookup docker-registry.docker-registry.svc.cluster.local: no such host
```

Well, that's not working: `dial tcp: lookup docker-registry.docker-registry.svc.cluster.local: no such host`.

In conclusion: the docker registry name must be resolvable from the node.

It's always DNS. We'll pick this up next time.
