---
title: "Kubernetes API from bash"
date: 2025-03-27T17:27Z
short_title: "Introduction"
tags: kubernetes curl bash
layout: series
series: k8s-bash
---

You can use the Kubernetes API from bash (using curl). Here's how. I've touched on this topic in the past, but I wanted
to collect everything in one place.

Here's how to access the Kubernetes API from bash, in a pod, using curl.

```
kubectl run \
    --rm --stdin --tty \
    --image=ubuntu-extra --image-pull-policy=Never \
    ubuntu-extra
```

Inside a pod, the Kubernetes API server is identified by the KUBERNETES_SERIVCE_HOST and KUBERNETES_SERVICE_PORT environment variables.

```
# curl https://${KUBERNETES_SERVICE_HOST}:${KUBERNETES_SERVICE_PORT}/api/v1/nodes
curl: (60) SSL certificate problem: unable to get local issuer certificate
```

use --insecure


```
# curl --insecure https://${KUBERNETES_SERVICE_HOST}:${KUBERNETES_SERVICE_PORT}/api/v1/nodes
{
  "kind": "Status",
  "apiVersion": "v1",
  "metadata": {},
  "status": "Failure",
  "message": "nodes is forbidden: User \"system:anonymous\" cannot list resource \"nodes\" in API group \"\" at the cluster scope",
  "reason": "Forbidden",
  "details": {
    "kind": "nodes"
  },
  "code": 403
}
```

anonymous because we didn't provide a token.

also fix the insecure (maybe just skip the above bit)

```
# curl \
    --cacert /var/run/secrets/kubernetes.io/serviceaccount/ca.crt \
    --header "Authorization: Bearer $(</var/run/secrets/kubernetes.io/serviceaccount/token)" \
    https://${KUBERNETES_SERVICE_HOST}:${KUBERNETES_SERVICE_PORT}/api/v1/nodes
{
  "kind": "Status",
  "apiVersion": "v1",
  "metadata": {},
  "status": "Failure",
  "message": "nodes is forbidden: User \"system:serviceaccount:default:default\" cannot list resource \"nodes\" in API group \"\" at the cluster scope",
  "reason": "Forbidden",
  "details": {
    "kind": "nodes"
  },
  "code": 403
}   
```

```
kubectl --namespace default create serviceaccount cluster-admin
kubectl create clusterrolebinding cluster-admin:cluster-admin --serviceaccount=default:cluster-admin --clusterrole=cluster-admin
```

```
kubectl run \
    --rm \
    --stdin --tty \
    --image=ubuntu-extra --image-pull-policy=Never \
    --overrides='{"spec": {"serviceAccount": "cluster-admin"}}' \
    ubuntu-extra
```

works:

```
# curl \
    --cacert /var/run/secrets/kubernetes.io/serviceaccount/ca.crt \
    --header "Authorization: Bearer $(</var/run/secrets/kubernetes.io/serviceaccount/token)" \
    https://${KUBERNETES_SERVICE_HOST}:${KUBERNETES_SERVICE_PORT}/api/v1/nodes
```

add silent, limit, jq:

```
# curl \
    --silent \
    --cacert /var/run/secrets/kubernetes.io/serviceaccount/ca.crt \
    --header "Authorization: Bearer $(</var/run/secrets/kubernetes.io/serviceaccount/token)" \
    https://${KUBERNETES_SERVICE_HOST}:${KUBERNETES_SERVICE_PORT}/api/v1/nodes?limit=500 | jq -r '.items[].metadata.name'
docker-desktop
```

See also _posts/2022/2022-01-07-k8s-api-in-container.md

To do the same from outside the cluster, we need a config file. Here's how to use docker desktop's:

```
current_context=$(<$KUBECONFIG yq '.current-context')
<$KUBECONFIG yq ".contexts[] | select(.name == \"$current_context\").context.cluster"
```

or use export and strenv.

or complicated shit (which assumes we're using client certs, which is the case for docker desktop and k3s, but not EKS or teleport).

```
kubernetes_ca=$(<$KUBECONFIG yq '(.current-context as $c | .contexts[] | select(.name == $c).context.cluster) as $cluster | .clusters[] | select(.name == $cluster).cluster.certificate-authority-data' | base64 -d)

kubernetes_client_certificate=$(<$KUBECONFIG yq '(.current-context as $c | .contexts[] | select(.name == $c).context.user) as $user | .users[] | select(.name == $user).user.client-certificate-data' | base64 -d)

kubernetes_client_key=$(<$KUBECONFIG yq '(.current-context as $c | .contexts[] | select(.name == $c).context.user) as $user | .users[] | select(.name == $user).user.client-key-data' | base64 -d)
```

```
% curl --silent --cacert <(echo $kubernetes_ca) --cert <(echo $kubernetes_client_cert) --key <(echo $kubernetes_client_key) "$kubernetes_server/api/v1/nodes?limit=500" | jq -r '.items[].metadata.name'
docker-desktop
```

{% include _series_toc.html %}
