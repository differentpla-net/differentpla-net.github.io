---
title: "Kubernetes Leases"
date: 2022-01-24T19:27:00Z
tags: kubernetes
---

Sometimes you need a singleton service. Here's an investigation of using Kubernetes Lease objects to achieve this.
Unfortunately, all of the example code I've found relies on the [`leaderelection` package](https://pkg.go.dev/k8s.io/client-go/tools/leaderelection),
which is only relevant if you're programming in Go. So let's dig into what's actually going on and then see if we
can make it work from Elixir.

There are a bunch of different models for leader election. This one's relatively easy to understand, and makes use of the Kubernetes `Lease` object.

It goes something like this:

1. The `Lease` object is owned by the leader, and anyone can query to find out the current leader.
2. Periodically, the leader updates the object, to state "I'm still the leader".
3. If that doesn't happen, the other nodes scramble to take ownership of the lease object.

It's a bit more complicated that that, but it might be good enough for our purposes.

## Create the lease object

The API documentation is at <https://kubernetes.io/docs/reference/kubernetes-api/cluster-resources/lease-v1/>.



## Alternatives

Other alternatives are to use (e.g.) consul; see
<https://learn.hashicorp.com/tutorials/consul/application-leader-elections> or maybe etcd.

Or you could go with a full-blown Paxos or Raft implementation. Those last two are more for shared consensus, rather
than simple leadership.

## Notes

```
$ kubectl proxy
```

```
$ curl http://localhost:8001/apis/coordination.k8s.io/v1/namespaces/default/leases
```

```json
{
  "kind": "LeaseList",
  "apiVersion": "coordination.k8s.io/v1",
  "metadata": {
    "resourceVersion": "4351933"
  },
  "items": []
}
```


```
curl X POST http://localhost:8001/apis/coordination.k8s.io/v1/namespaces/default/leases \
   -H 'Content-Type: applicatiion/json' \
   -d '{"metadata":{"name": "my-lease"}}'
```


```json
{
  "kind": "Lease",
  "apiVersion": "coordination.k8s.io/v1",
  "metadata": {
    "name": "my-lease",
    "namespace": "default",
    "uid": "4af3c10f-a7e3-41cf-983d-9ac5c1ca0d39",
    "resourceVersion": "4352272",
    "creationTimestamp": "2022-01-24T19:46:26Z"
  },
  "spec": {

  }
}
```

If you try to create a lease with the same name, you get 409 Conflict, and the following:

```json
{
  "kind": "Status",
  "apiVersion": "v1",
  "metadata": {

  },
  "status": "Failure",
  "message": "leases.coordination.k8s.io \"my-lease\" already exists",
  "reason": "AlreadyExists",
  "details": {
    "name": "my-lease",
    "group": "coordination.k8s.io",
    "kind": "leases"
  },
  "code": 409
}
```

Delete a lease with:

```
curl X DELETE http://localhost:8001/apis/coordination.k8s.io/v1/namespaces/default/leases/my-lease
```

Update a lease:

```

```

The magic is in the `resourceVersion`, which guarantees that you don't get into a last-write-wins situation:

```
curl -X PUT http://localhost:8001/apis/coordination.k8s.io/v1/namespaces/default/leases/my-lease -H 'Content-Type: application/json' -d '{"metadata":{"name": "my-lease", "resourceVersion": "12345"}}' ; echo
{
  "kind": "Status",
  "apiVersion": "v1",
  "metadata": {

  },
  "status": "Failure",
  "message": "Operation cannot be fulfilled on leases.coordination.k8s.io \"my-lease\": the object has been modified; please apply your changes to the latest version and try again",
  "reason": "Conflict",
  "details": {
    "name": "my-lease",
    "group": "coordination.k8s.io",
    "kind": "leases"
  },
  "code": 409
}
```

```
curl -X GET http://localhost:8001/apis/coordination.k8s.io/v1/namespaces/default/leases/my-lease
{
  "kind": "Lease",
  "apiVersion": "coordination.k8s.io/v1",
  "metadata": {
    "name": "my-lease",
    "namespace": "default",
    "uid": "4af3c10f-a7e3-41cf-983d-9ac5c1ca0d39",
    "resourceVersion": "4352272",
    "creationTimestamp": "2022-01-24T19:46:26Z"
  },
  "spec": {

  }
}
```

So, if you update with the resourceVersion that you saw, you're gonna guarantee that you don't stomp on someone else's changes.

It goes like this:

GET the lease. It tells you who the leader is. If you think the leader's dead, then try updating it to set yourself as the leader. If you win, you're the leader.

The problem then becomes one of deciding when the leader's dead. That has the leader update the lease with a timestamp (caution: leader's time, not yours). If it hasn't been updated in a while, then the leader's dead. Not sure (yet) how that works.

## Crazy Idea: LiveBook on k3s

Docs say:

```
docker run -p 8080:8080 --pull always livebook/livebook
```

So that's:

```
kubectl create namespace livebook
```

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  labels:
    app: livebook
  name: livebook
  namespace: livebook
spec:
  replicas: 1
  selector:
    matchLabels:
      app: livebook
  template:
    metadata:
      labels:
        app: livebook
    spec:
      containers:
      - name: livebook
        image: livebook/livebook:0.5.1
        env:
        - name: LIVEBOOK_PORT
          value: "8080"
```

Since we want to be able to access the replicas individually, we're gonna have a few problems: we'll need to use a headless service and access the IP addresses individually, but they won't be accessible externally, maybe. A NodePort ain't gonna work, since it'll load-balance to any of the replicas, and a ClusterIP is only accessible internally. Plus: it only refers to the service. I suspect that we'll need a headless service and a statefulset.

Apparently, you can only create a `NodePort` service with a selector from YAML:

```yaml
apiVersion: v1
kind: Service
metadata:
  labels:
    app: livebook
  name: livebook
  namespace: livebook
spec:
  type: NodePort
  ports:
  - port: 8080
    protocol: TCP
    targetPort: 8080
  selector:
    app: livebook
```

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: lease-admin
  namespace: livebook
rules:
- apiGroups:
  - coordination.k8s.io
  resources:
  - leases
  verbs:
  - get
  - list
  - create
  - update
  - delete
```

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: default:lease-admin
  namespace: livebook
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: Role
  name: lease-admin
subjects:
- kind: ServiceAccount
  name: default
  namespace: livebook
```


On the bright side, this is Elixir; we can just have a leader election between processes on the same node. That'll do, pig.

`create deployment`, above, uses `app: livebook` for the label; none of this fancy new stuff.

default service account doesn't have enough mojo. Need to figure that out.

Aha! Clock drift: the values in the record are ignored; the client records when it last pulled the record, and then checks to see if it's _changed_ since then. So, unless the leader is making use of those timestamps, they could just be randomly-generated values. I guess timestamps are useful for humans, though.

Also: fencing? What?

- If we have a single replica, then k8s deals with recovery, and we don't need leader elections.
  - How does this work with a deploy? How do we transfer ownership to the new pod?
  - Otherwise, leadership elections can occur by using k8s leases.
    - <https://kubernetes.io/blog/2016/01/simple-leader-election-with-kubernetes/>
    - <https://itnext.io/leader-election-in-kubernetes-using-client-go-a19cbe7a9a85>
    - <https://pkg.go.dev/k8s.io/client-go/tools/leaderelection>
    - <https://github.com/kubernetes/client-go/blob/56656ba0e04ff501549162385908f5b7d14f5dc8/tools/leaderelection/leaderelection.go#L265>
    - <https://docs.sysdig.com/en/docs/installation/sysdig-agent/agent-installation/using-node-leases/>
