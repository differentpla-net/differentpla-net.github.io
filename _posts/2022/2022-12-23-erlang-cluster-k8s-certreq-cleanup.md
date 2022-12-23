---
title: "Erlang cluster on Kubernetes: CertificateRequest cleanup"
short_title: "CertificateRequest cleanup"
date: 2022-12-23T17:36:00.000Z
layout: series
series: erlang-cluster-k8s
tags: kubernetes cert-manager
---

In [this post]({% post_url 2022/2022-12-22-erlang-cluster-k8s-certificate-requests-cert-manager %}), I showed how to use
an init container to create `CertificateRequest` objects, which _cert-manager_ signs, returning the certificates. A new
request is created every time a pod starts. This eventually leaves a lot of stale `CertificateRequest` objects. We
should clean those up.

I opted to use a CronJob.

## Dockerfile

The Dockerfile is very similar to the init container:

```dockerfile
FROM docker.io/alpine

# We need coreutils for a version of 'date' that can do --date '15 minutes ago'.
# We need curl and jq to access the Kubernetes API and to parse the responses.
RUN apk add --no-cache coreutils && \
    apk add --no-cache curl && \
    apk add --no-cache jq

WORKDIR /erlclu-request-cleanup
COPY erlclu-request-cleanup.sh erlclu-request-cleanup.sh

ENTRYPOINT ["/erlclu-request-cleanup/erlclu-request-cleanup.sh"]
```

## erlclu-request-cleanup.sh

The cleanup script looks like this:

```sh
#!/bin/sh

expiry=$(date --date="${MAX_AGE_MINS} minutes ago" --utc +"%Y-%m-%dT%H:%M:%SZ")

AUTH_TOKEN="$(cat /var/run/secrets/kubernetes.io/serviceaccount/token)"
NAMESPACE="$(cat /var/run/secrets/kubernetes.io/serviceaccount/namespace)"

CA_CERT_BUNDLE=/var/run/secrets/kubernetes.io/serviceaccount/ca.crt

cert_manager_api_base_url="https://${KUBERNETES_SERVICE_HOST}:${KUBERNETES_SERVICE_PORT}/apis/cert-manager.io/v1"
certificate_requests_base_url="${cert_manager_api_base_url}/namespaces/${NAMESPACE}/certificaterequests"

certificate_requests=$(curl -s -X GET \
        --header "Accept: application/json" \
        --header "Authorization: Bearer ${AUTH_TOKEN}" \
        --cacert "${CA_CERT_BUNDLE}" \
        "${certificate_requests_base_url}?labelSelector=app=${APPLICATION_LABEL}")

expired_requests=$(echo "$certificate_requests" | \
    jq --arg expiry "$expiry" -r '.items[] | select(.metadata.creationTimestamp < $expiry) | .metadata.name')

for x in $expired_requests; do
    curl -s -X DELETE \
        --header "Authorization: Bearer ${AUTH_TOKEN}" \
        --cacert "${CA_CERT_BUNDLE}" \
        "${certificate_requests_base_url}/$x" --output /dev/null
done
```

1. It works out an expiry time for certificates (e.g. 15 minutes ago); we need the `coreutils` version of `date`, rather
   than the busybox one, for this to work.
2. It queries for all of our `CertificateRequest` objects.
3. It runs that through `jq` to find those that have expired.
4. It deletes each of the expired objects.

By looking for requests that are at least 15 minutes old, we don't accidentally delete those that are still in the
process of being issued. This assumes that issuing a certificate takes less than 15 minutes; I'm comfortable with that.

## CronJob

The `CronJob` looks like this:

```yaml
apiVersion: batch/v1
kind: CronJob
metadata:
  name: erlclu-request-cleanup
  namespace: erlclu
spec:
  schedule: "8/15 * * * *"
  successfulJobsHistoryLimit: 1
  failedJobsHistoryLimit: 1
  concurrencyPolicy: Forbid
  jobTemplate:
    spec:
      template:
        spec:
          containers:
            - name: request-cleanup
              image: docker.k3s.differentpla.net/erlclu-request-cleanup:0.1.0
              imagePullPolicy: Always
              env:
                - name: APPLICATION_LABEL
                  value: erlclu
                - name: MAX_AGE_MINS
                  value: "15"
          restartPolicy: OnFailure
          activeDeadlineSeconds: 90
          serviceAccountName: erlclu
```

It runs every 15 minutes, starting at 8 minutes past the hour: H+8, H+23, H+38, H+53.

I've set `successfulJobsHistoryLimit` and `failedJobsHistoryLimit` to `1`, meaning that you can look at the logs
afterwards. On the other hand, it does leave the pod lying around, which is untidy.

## Service account

When we originally defined the `erlclu` service account, we only granted it `create` and `get` permissions; we need to
add `list` and `delete`:

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  namespace: erlclu
  name: certificate-requester
rules:
- apiGroups: ["cert-manager.io"]
  resources: ["certificaterequests"]
  verbs: ["create", "get", "list", "delete"]
```
