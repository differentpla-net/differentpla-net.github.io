---
title: "Erlang cluster on Kubernetes: tcpdump"
short_title: "tcpdump"
date: 2022-12-22T10:04:00.000Z
layout: series
series: erlang-cluster-k8s
---

TODO: tcpdump

```
kubectl --namespace erlclu debug --quiet -i erlclu-7d86f49786-trrx8 \
    --target=erlclu --image=nicolaka/netshoot -- \
        tcpdump -i eth0 -s 65535 -w - > dump.pcap
```

Do NOT include `-t` in `-it`, and DO include `--quiet`, otherwise various human-readable stuff gets written, which
confuses Wireshark.

kubectl debug doesn't allow for security contexts, so tcpdump doesn't work if you have runAsNonRoot, etc.

- <https://kubernetes.io/docs/tasks/configure-pod-container/security-context/>

That's just a command-line limitation, though. The EphemeralContainers spec allows for securitycontext; see <https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.26/#ephemeralcontainer-v1-core>.

kubectl -v6 reveals that it just PATCH the pod:

```
I0110 18:44:20.683484   16438 round_trippers.go:553] PATCH https://roger-nuc0:6443/api/v1/namespaces/erlclu/pods/erlclu-6ddc9c7847-fvfsq/ephemeralcontainers 200 OK in 22 milliseconds
```

Then it watches the pod to see when the container comes up:

```
I0110 18:44:20.685477   16438 reflector.go:221] Starting reflector *v1.Pod (0s) from vendor/k8s.io/client-go/tools/watch/informerwatcher.go:146
I0110 18:44:20.685524   16438 reflector.go:257] Listing and watching *v1.Pod from vendor/k8s.io/client-go/tools/watch/informerwatcher.go:146
I0110 18:44:20.690168   16438 round_trippers.go:553] GET https://roger-nuc0:6443/api/v1/namespaces/erlclu/pods?fieldSelector=metadata.name%3Derlclu-6ddc9c7847-fvfsq&limit=500&resourceVersion=0 200 OK in 4 milliseconds
I0110 18:44:20.691019   16438 debug.go:747] watch received event "ADDED" with object *v1.Pod
I0110 18:44:20.696434   16438 round_trippers.go:553] GET https://roger-nuc0:6443/api/v1/namespaces/erlclu/pods?allowWatchBookmarks=true&fieldSelector=metadata.name%3Derlclu-6ddc9c7847-fvfsq&resourceVersion=13734348&timeout=9m14s&timeoutSeconds=554&watch=true 200 OK in 5 milliseconds
I0110 18:44:20.710603   16438 debug.go:747] watch received event "MODIFIED" with object *v1.Pod
I0110 18:44:20.710648   16438 debug.go:762] debug container status is &ContainerStatus{Name:debugger-bcml9,State:ContainerState{Waiting:&ContainerStateWaiting{Reason:PodInitializing,Message:,},Running:nil,Terminated:nil,},LastTerminationState:ContainerState{Waiting:nil,Running:nil,Terminated:nil,},Ready:false,RestartCount:0,Image:docker.io/library/alpine:latest,ImageID:,ContainerID:,Started:nil,}
I0110 18:44:21.998645   16438 debug.go:747] watch received event "MODIFIED" with object *v1.Pod
I0110 18:44:21.998753   16438 debug.go:762] debug container status is &ContainerStatus{Name:debugger-bcml9,State:ContainerState{Waiting:nil,Running:&ContainerStateRunning{StartedAt:2023-01-15 11:58:13 +0000 GMT,},Terminated:nil,},LastTerminationState:ContainerState{Waiting:nil,Running:nil,Terminated:nil,},Ready:false,RestartCount:0,Image:docker.io/library/alpine:latest,ImageID:docker.io/library/alpine@sha256:f271e74b17ced29b915d351685fd4644785c6d1559dd1f2d4189a5e851ef753a,ContainerID:containerd://76c34201c53f831f39c3cbde4fa1d3f1330809751e41a4396056da13518666ff,Started:nil,}
I0110 18:44:21.999068   16438 reflector.go:227] Stopping reflector *v1.Pod (0s) from vendor/k8s.io/client-go/tools/watch/informerwatcher.go:146
```

Then it transitions to exec/debug mode:

```
I0110 18:44:22.030787   16438 round_trippers.go:553] POST https://roger-nuc0:6443/api/v1/namespaces/erlclu/pods/erlclu-6ddc9c7847-fvfsq/attach?container=debugger-bcml9&stdin=true&stdout=true&tty=true 101 Switching Protocols in 27 milliseconds
```

So:
1. can we patch the pod ourselves, but include a different security context?
2. can we connect to an existing ephemeral container using kubectl?

```
% kubectl --namespace erlclu get pod erlclu-6ddc9c7847-fvfsq -o json | jq '.status.ephemeralContainerStatuses[].name'
"debugger-bcml9"
"debugger-kvvpq"
"debugger-vv2z4"
```

```
% kubectl --namespace erlclu exec -it erlclu-6ddc9c7847-fvfsq -c debugger-bcml9 -- /bin/sh
error: unable to upgrade connection: container not found ("debugger-bcml9")
```

:sad-face-emoji:

...because it's terminated:

```
kubectl --namespace erlclu get pod erlclu-6ddc9c7847-fvfsq -o json | jq '.status.ephemeralContainerStatuses[]'
```

```json
{
  "containerID": "containerd://37c8e45cb9310512d661c6f50208dc47edde0e4b4358f4f17799e74d76d9cf90",
  "image": "docker.io/library/alpine:latest",
  "imageID": "docker.io/library/alpine@sha256:f271e74b17ced29b915d351685fd4644785c6d1559dd1f2d4189a5e851ef753a",
  "lastState": {},
  "name": "debugger-vv2z4",
  "ready": false,
  "restartCount": 0,
  "state": {
    "terminated": {
      "containerID": "containerd://37c8e45cb9310512d661c6f50208dc47edde0e4b4358f4f17799e74d76d9cf90",
      "exitCode": 130,
      "finishedAt": "2023-01-15T11:58:07Z",
      "reason": "Error",
      "startedAt": "2023-01-15T11:55:25Z"
    }
  }
}
```

But we can do `attach false` -- <https://devopstales.github.io/home/k8s-ephemerald-pod/> -- to avoid terminating it.

So the question becomes: can we do the following?

1. PATCH the pod spec, specifying an ephemeral container, with a securityContext that allows tcpdump?
2. Wait until the container is started.
3. Run kubectl attach to connect to it.

Or, more specifically, can we do the above to run tcpdump and -- rather than attach -- just use the POST thing from above to pull stdout ourselves?

And can we turn that into a kubectl plugin?
- https://kubernetes.io/docs/tasks/extend-kubectl/kubectl-plugins/
- https://laiyuanyuan-sg.medium.com/build-a-kubectl-plugin-from-scratch-34daa9de15fd

Or is there one that exists already?
- https://krew.sigs.k8s.io/
- there exists https://github.com/eldadru/ksniff, which seems to do the correct thing, albeit in a weird way when it comes to `-p` privileged. Probably because it doesn't know about `kubectl debug` (per the readme) and therefore probably pre-dates ephemeral containers.
