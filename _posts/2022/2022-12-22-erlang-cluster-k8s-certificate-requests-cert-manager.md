---
title: "Erlang cluster on Kubernetes: Submitting certificate requests to cert-manager"
short_title: "Submitting CSRs to cert-manager"
date: 2022-12-22T10:01:00.000Z
layout: series
series: erlang-cluster-k8s
---

In the [previous post]({% post_url 2022/2022-12-22-erlang-cluster-k8s-certificate-requests-openssl %}) we used OpenSSL
to create a certificate signing request. In this post, we'll submit it to _cert-manager_ and get the certificate back.

## Service account

We're going to access the cert-manager API from inside the init container. To do that we'll either need to change the
permissions of the "default" service account, or we'll need to create a new service account. I opted for the latter.

Since all containers in a pod run with the same service account, I named it `erlclu`:

```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: erlclu
  namespace: erlclu
```

We need a role that can request certificates. It needs `create` (to submit the request) and `get` (to retrieve the
generated certificate):

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  namespace: erlclu
  name: certificate-requester
rules:
- apiGroups: ["cert-manager.io"]
  resources: ["certificaterequests"]
  verbs: ["create", "get"]
```

...and we need a role binding:

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: request-certificate
  namespace: erlclu
subjects:
- kind: ServiceAccount
  name: erlclu
roleRef:
  kind: Role
  name: certificate-requester
  apiGroup: rbac.authorization.k8s.io
```

## Accessing the Kubernetes API from a container

I wrote [something]({% post_url 2022/2022-01-07-k8s-api-in-container %}) about this a while ago. We'll need some preliminaries:

```bash
AUTH_TOKEN="$(cat /var/run/secrets/kubernetes.io/serviceaccount/token)"
NAMESPACE="$(cat /var/run/secrets/kubernetes.io/serviceaccount/namespace)"

CA_CERT_BUNDLE=/var/run/secrets/kubernetes.io/serviceaccount/ca.crt

cert_manager_api_base_url="https://${KUBERNETES_SERVICE_HOST}:${KUBERNETES_SERVICE_PORT}/apis/cert-manager.io/v1"
certificate_requests_base_url="${cert_manager_api_base_url}/namespaces/${NAMESPACE}/certificaterequests"
```

## Submitting the CSR

We need to base64-encode the CSR:

```bash
encoded_csr=$(base64 -w0 < "$CERTS_DIR/$CERT_FILENAME.csr")
```

...we need a name for the `CertificateRequest`; I'll use the pod name:

```bash
request_name="$(hostname -s)"
```

Then we can actually make the request. We use a heredoc to build the JSON body. Piping heredocs is a little weird, but
it looks like this:

```bash
cat << EOF | \
    curl -s -X POST \
        --header "Content-Type: application/json" \
        --header "Authorization: Bearer ${AUTH_TOKEN}" \
        --cacert "${CA_CERT_BUNDLE}" \
        --data-binary @- \
        "${certificate_requests_base_url}"
{
    "apiVersion": "cert-manager.io/v1",
    "kind": "CertificateRequest",
    "metadata": {
        "name": "$request_name",
        "namespace": "$NAMESPACE",
        "labels": {
            "app": "$APPLICATION_LABEL"
        }
    },
    "spec": {
        "request": "$encoded_csr",
        "issuerRef": {
            "kind": "$ISSUER_KIND",
            "name": "$ISSUER_NAME"
        }
    }
}
EOF
```

The `APPLICATION_LABEL`, `ISSUER_KIND` and `ISSUER_NAME` variables are set in the K8s deployment. We'll see those below.

## Getting the new certificate

When _cert-manager_ signs the request, it will update the `.status` fields in the `CertificateRequest` object. Ideally,
we'd actually _poll_ the status. For now, however, a simple `sleep 5s` will do the job. I'll fix that later.

```bash
sleep 5s
```

Once polling (cough) tells us that the certificate has been signed successfully, we can write the CA and certificate to files in the shared `/certs` volume, where the main container can find them later:

```bash
res=$(curl -s \
    --header "Accept: application/json" \
    --header "Authorization: Bearer ${AUTH_TOKEN}" \
    --cacert "${CA_CERT_BUNDLE}" \
    "${certificate_requests_base_url}/$request_name")

# Write the cert and the CA to files.
echo "$res" | jq -r '.status.ca' | base64 -d > "$CERTS_DIR/ca.crt"
echo "$res" | jq -r '.status.certificate' | base64 -d > "$CERTS_DIR/$CERT_FILENAME.crt"
```

Note that this required us to add `apk add openssl curl jq` to the docker container.

## Updating the deployment

We need to specify the following:

- The service account
- Environment variables that the `erlclu-init.sh` script is expecting.
- The volume mount for the shared `/certs` volume.

```yaml
spec:
  serviceAccountName: erlclu
  initContainers:
    - name: erlclu-init
      #...
      env:
        - name: MY_POD_IP
          valueFrom:
            fieldRef:
              fieldPath: status.podIP
        - name: APPLICATION_LABEL
          value: erlclu
        - name: ISSUER_KIND
          value: ClusterIssuer
        - name: ISSUER_NAME
          value: k3s-ca-cluster-issuer
        - name: CERTS_DIR
          value: /certs
        - name: CERT_FILENAME
          value: tls-dist
      volumeMounts:
        - name: tls-dist
          mountPath: /certs
```

Note that it's using the `ClusterIssuer` that [I created]({% post_url 2022/2022-02-06-cert-manager %}) for
`*.k3s.differentpla.net`. I'll fix that later.

`APPLICATION_LABEL` is used to differentiate our `CertificateRequest` objects from the ones that _cert-manager_ uses for
generating TLS certficates. We'll use it later to ensure we only clean up our requests.

## Shared /certs volume

The certificates are shared between the init container and the main container, and nothing else. Because of this, I used
a memory-backed `emptyDir` volume:

```yaml
spec:
  #...
  volumes:
    - name: tls-dist
      emptyDir:
        medium: "Memory"
        sizeLimit: 32Ki
```

`32Ki` seems to be large enough. The certificate and CA are about 24KiB in size.

For the time-being, I left the existing certificates in `/secrets`. I'll describe how I switched to using the new
certificates and got rid of the old ones in the next post.
