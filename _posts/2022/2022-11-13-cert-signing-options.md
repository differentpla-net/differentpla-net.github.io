---
title: "Options for automatically creating certificates for mutual pod authentication"
date: 2022-11-13T15:04:00Z
tags: kubernetes certificates
---

I want to set up an Erlang cluster in Kubernetes, using TLS with mutual authentication. This post discusses some of the
potential options for doing that. It's also applicable to general mutual TLS between pods.

When doing mutual TLS authentication between pods, we need each pod to have its own certificate. It will use this
certificate both as a server certificate and as a client certificate. All of the certificates should be issued by the
same certificate authority.

The problem is that we can't easily create the certificates ahead of time, because the pod names aren't known at that
point. Moreover, the usual way to make keys and certificates available to containers is as Secrets, which aren't
pod-specific.

## Certificate Signing Requests

Kubernetes has an in-built
[CertificateSigningRequest](https://kubernetes.io/docs/reference/access-authn-authz/certificate-signing-requests/)
resource. Extremely simplified, it works like this:

1. Create a `CertificateSigningRequest` resource.
2. A controller in the cluster watches for these.
3. It signs the request and puts the result in the `status.certificate` field.

This is explained in more detail (and with more manual steps) here:
[Manage TLS Certificates in a Cluster](https://kubernetes.io/docs/tasks/tls/managing-tls-in-a-cluster/).

My initial plan was to have an [init container](https://kubernetes.io/docs/concepts/workloads/pods/init-containers/) in
the pod which would submit the `CertificateSigningRequest` and would wait for it to be signed. It would then write that
to a shared volume for the main container to find.

The problem with this scheme is that Kubernetes only includes controllers for specific types of
CertificateSigningRequest resources, mostly for kubelet mutual authentication, and it doesn't support general
certificate use cases.

Nitpicking: you _can_ enable a default certificate signer (see
[Configuring your cluster to provide signing](https://kubernetes.io/docs/tasks/tls/managing-tls-in-a-cluster/#configuring-your-cluster-to-provide-signing))
on the [Manage TLS Certificates in a Cluster](https://kubernetes.io/docs/tasks/tls/managing-tls-in-a-cluster/) page),
but it only allows for a single CA, and it requires the CA key and certificate live somewhere on the bare metal running
the cluster.

I haven't done much investigation, but it's not clear to me how you get that configured on, e.g., EKS or AKS, or -- in
my case -- on K3s. It also seems like a thing that your DevSecOps team are going to have a conniption fit over.

I could probably write my own controller to implement this, but that seems like a lot of effort. It also seems that
`CertificateSigningRequest` resources are cluster-wide (they're not namespaced), which means any such controller would
need cluster-wide permissions. Again, your DevSecOps team are likely to be asking pointed questions about that.

So that's probably out.

## cert-manager

Then it occurred to me that I've got cert-manager installed on my cluster. That _does_ support general certificate use
cases. In fact, that's what it's _for_. Can I leverage that at all?

At its most basic, cert-manager watches for `Certificate` resources, which specify various certificate attributes, and
it creates (and keeps in sync) a `Secret` resource containing the generated certificate for each one.

The problem with that is that `Secret` resources aren't pod-specific, and we need a unique certificate for each pod.

But it _does_ also provide [`CertificateRequest`](https://cert-manager.io/docs/concepts/certificaterequest/) resources,
which are functionally the same as `CertificateSigningRequest` resources.

To see it in action, you can follow the [Normal User](https://kubernetes.io/docs/reference/access-authn-authz/certificate-signing-requests/#normal-user) example in the Kubernetes _"Certificate Signing Requests"_ documentation. But instead of a Kubernetes `CertificateSigningRequest`, create a cert-manager `CertificateRequest` instead:

```
cat <<EOF | kubectl apply -f -
apiVersion: cert-manager.io/v1
kind: CertificateRequest
metadata:
  name: myuser
spec:
  request: LS0tLS1...
  isCA: false
  usages:
  - server auth
  - client auth
  # 90 days
  duration: 2160h
  issuerRef:
    name: k3s-ca-cluster-issuer
    kind: ClusterIssuer
EOF
```

### Other Options

- cert-manager provides a CSI Driver specifically for mTLS of Pods. See <https://cert-manager.io/docs/usage/csi/>. The
  documentation seems a bit sparse and I've not had a chance to play with it yet.
- There's an experimental feature for cert-manager that allows it to process Kubernetes-native
  `CertificateSigningRequest` objects; see <https://cert-manager.io/docs/usage/kube-csr/>. This would allow you to use
  an init container, but without _directly_ adding a dependency on cert-manager.

## Conclusions

To support mutual authentication between pods, I'm going to go with the following plan:

- Assume cert-manager.
- Each pod has an init container that:
  1. Creates cert-manager `CertificateRequest` objects.
  2. Waits for the certificate to be ready.
  3. Writes the certificate to a shared volume.
- Use that certificate for mutual TLS authentication.

## Open Questions

- Garbage collection. Someone's got to delete the `CertificateRequest` objects.
  - CronJob, maybe?
- Expiry and rotation. The example above has 90 day duration. When that expires, who's going to rotate it?
  - Probably a sidecar container in addition to/instead of the init container.
- ...etc.
