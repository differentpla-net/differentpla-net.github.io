---
title: "k3s on Raspberry Pi: NodePort with multiple replicas"
short_title: "NodePort with multiple replicas"
date: 2021-12-30T12:10:00Z
layout: series
series: k3s
tags: k3s raspberry-pi
---

If you're using a [NodePort]({% post_url 2021/2021-12-17-k3s-nodeport %}) service, and it has multiple replicas, how does it know which replica to use?

Let's try it by running a [simple node.js server]({% post_url 2020/2020-02-06-k3s-raspi-node-server %}) with multiple replicas.

We can find out which node we're running on by following the instructions at <https://kubernetes.io/docs/tasks/inject-data-application/environment-variable-expose-pod-information/>.

## server.js

```javascript
const PORT = process.env.PORT || 8111;

const MY_NODE_NAME = process.env.MY_NODE_NAME;
const MY_POD_NAMESPACE = process.env.MY_POD_NAMESPACE;
const MY_POD_NAME = process.env.MY_POD_NAME;
const MY_POD_IP = process.env.MY_POD_IP;

const http = require('http');
const server = http.createServer((req, res) => {
        res.statusCode = 200;
        res.setHeader('Content-Type', 'text/plain');

        const info = `pod ${MY_POD_NAME} on node ${MY_NODE_NAME}\n`;
        res.end(info);
});

server.listen(PORT);
console.log(`Server listening on port ${PORT}`);
```

## Dockerfile

```dockerfile
FROM node:17
EXPOSE 8111
COPY server.js .
CMD ["node", "server.js"]
```

## Build it

```bash
DOCKER_REGISTRY=docker.k3s.differentpla.net
IMAGE_TAG="$(date +%s)"
docker build -t node-server . \
&& docker tag node-server "${DOCKER_REGISTRY}/node-server:${IMAGE_TAG}" \
&& docker push "${DOCKER_REGISTRY}/node-server:${IMAGE_TAG}"
```

## Deploy it

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: node-server
  labels:
    app: node-server
spec:
  replicas: 1
  selector:
    matchLabels:
      app: node-server
  template:
    metadata:
      labels:
        app: node-server
        name: node-server
    spec:
      containers:
      - name: node-server
        image: docker.k3s.differentpla.net/node-server:1640867226
        env:
        - name: MY_NODE_NAME
          valueFrom:
            fieldRef:
              fieldPath: spec.nodeName
        - name: MY_POD_NAME
          valueFrom:
            fieldRef:
              fieldPath: metadata.name
        - name: MY_POD_NAMESPACE
          valueFrom:
            fieldRef:
              fieldPath: metadata.namespace
        - name: MY_POD_IP
          valueFrom:
            fieldRef:
              fieldPath: status.podIP
```

Note that the image tag needs to be changed if you want to redeploy, though there are ways around this. I'm using the current time (`date +%s`). This is a bit shonky. I'll explore CI/CD options at some point.

```yaml
apiVersion: v1
kind: Service
metadata:
  labels:
    app: node-server
  name: node-server
spec:
  type: NodePort
  ports:
  - port: 8111
    protocol: TCP
    targetPort: 8111
  selector:
    app: node-server
```

```bash
kubectl apply -f deployment.yaml -f svc.yaml
```

## Where is it running?

```
$ curl http://rpi401:30184
pod node-server-fcb84c684-xngdm on node rpi402
```

Currently we've got a single instance, and it's running on `rpi402`. We can confirm this with `kubectl get pods --selector=app=node-server -o wide`, or even with `kubectl get pods --selector=app=node-server -o jsonpath='{.items[*].spec.nodeName}'`.

## Scale it up

We've got 5 nodes; let's scale up to 3 replicas:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: node-server
  labels:
    app: node-server
spec:
  replicas: 3
...
```

```
$ kubectl apply -f deployment.yaml
```

```
$ kubectl get pods --selector=app=node-server -o jsonpath='{range .items[*]}{.spec.nodeName}{"\n"}{end}'
rpi402
rpi402
rpi401
```

Note that two of the pods are running on the same node. I'll look into [anti-affinity](https://kubernetes.io/docs/concepts/scheduling-eviction/assign-pod-node/#affinity-and-anti-affinity) at some point.

It doesn't display any preference for accessing the pod on the specified node:

```
$ curl http://rpi401:30184
pod node-server-fcb84c684-psf64 on node rpi401

$ curl http://rpi401:30184
pod node-server-fcb84c684-xngdm on node rpi402
```

To emphasise the point that the service is available on any node, not just the ones it's running on:

```
$ curl http://rpi404:30184
pod node-server-fcb84c684-psf64 on node rpi401
```

## References

- [How to Make Kubectl Jsonpath Output On Separate Lines](https://downey.io/notes/dev/kubectl-jsonpath-new-lines/)
