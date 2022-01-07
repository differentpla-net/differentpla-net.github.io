---
title: "Kubernetes API from inside a container"
date: 2022-01-07T18:15:00Z
tags: kubernetes
---

Messing around with the kubernetes API from inside a container.

## Create an 'admin' account in the 'default' namespace

```
kubectl --namespace default create serviceaccount admin
kubectl create clusterrolebinding admin:admin --serviceaccount=default:admin --clusterrole=admin
```

## Run an alpine container

```
kubectl run --stdin --tty --rm alpine --image=alpine --overrides='{"spec": {"serviceAccount": "admin"}}' -- sh
```

Of course, you could run an Ubuntu container. I'm kinda assuming that Alpine will start more quickly. If that bothers
you, drop the `--rm` and you can reattach to the pod later.

## Install some tools

```
apk add iputils curl jq
```

## Run curl

```
curl \
    --cacert /var/run/secrets/kubernetes.io/serviceaccount/ca.crt \
    --header "Authorization: Bearer $(cat /var/run/secrets/kubernetes.io/serviceaccount/token)" \
    https://${KUBERNETES_SERVICE_HOST}:${KUBERNETES_SERVICE_PORT}/api/v1/namespaces/default/endpoints | jq
```
## Alternatively

```
echo -n "Authorization: Bearer " > headers
cat /var/run/secrets/kubernetes.io/serviceaccount/token >> headers

export CURL_CA_BUNDLE=/var/run/secrets/kubernetes.io/serviceaccount/ca.crt

curl -H @headers \
    https://${KUBERNETES_SERVICE_HOST}:${KUBERNETES_SERVICE_PORT}/api/v1/namespaces/default/endpoints | jq
```

## Clean Up

```
kubectl delete clusterrolebinding admin:admin
kubectl --namespace default delete serviceaccount admin
```

## References

- <https://stackoverflow.com/questions/55940137/how-to-create-a-pod-that-uses-non-default-service-account-using-kubectl-run-comm>
- <https://stackoverflow.com/questions/49858027/cannot-query-kubernetes-unauthorized-endpoints-is-forbidden-user-cannot-list>
