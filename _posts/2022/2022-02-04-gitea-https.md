---
title: "Securing Gitea with HTTPS"
date: 2022-02-04T17:46:00Z
tags: gitea kubernetes
layout: series
series: k3s
---

I've got Gitea installed on my cluster, but it's currently accessed via HTTP (i.e. no TLS; it's not secure).

Ordinarily, I'd secure it by replacing the LoadBalancer service with an Ingress object. However: Gitea allows git access
over HTTPS and SSH protocols. If we use a standard Ingress object, we'll only be able to use HTTPS. That complicates
things.

## Options

### Separate DNS entries

While researching this problem, I found ["Deploying Fully Functional Gitea on Google Kubernetes Engine
(GKE)"][gitea-gke], which punts the problem: it runs HTTPS through an Ingress on one name (`gitea.example.com`), and SSH
through a LoadBalancer on another name (`git.example.com`).

[gitea-gke]: https://medium.com/swlh/deploying-fully-functional-gitea-on-google-kubernetes-engine-gke-c4134e13cc2e

### Traefik IngressRoute

In theory, however, Traefik IngressRoute objects support multiple protocols. My concern there, though, is how that's
going to work with DNS. I've got a LoadBalancer (on `192.168.28.10`) pointing at Traefik, which then uses the various
Ingress and IngressRoute objects to route the traffic appropriately.

I'd need to add port 22 (ssh) to that LoadBalancer and then somehow persuade Traefik to route _all_ SSH traffic to
Gitea. It would need to route _all_ traffic, because SSH doesn't provide the equivalent of HTTP's `Host` header or TLS
Server Name Indication (SNI).

### Bastion

I guess I could route the SSH traffic to a Bastion server (a "bounce box"), though... (strokes chin).

### Multiple Traefik Instances

One other option that occurs is to add [another Traefik instance][multiple-traefik], with its own LoadBalancer (hence
its own IP address and DNS entry). That would be responsible for Gitea and nothing else. It could route SSH traffic as
normal, and could do the TLS termination for Gitea.

[multiple-traefik]: https://blog.baeke.info/2019/07/01/quick-tip-deploying-multiple-traefik-ingresses/

### Gitea does TLS termination

The least-bad option, as far as I can tell at this point, is to have Gitea do its own TLS termination. That's documented
[here](https://docs.gitea.io/en-us/https-setup/); all we need to do is figure out how to provide the TLS certificates in
an idiomatic Kubernetes way.

## Gitea HTTPS setup

To tell Gitea about the TLS certificates, we need to edit the `values.yaml` file. As far as I can tell, that means uninstalling the Helm chart and then reinstalling it with the changes.

<div class="callout callout-warning" markdown="span">
The following doesn't remove the persistent volume used by Gitea, but you should probably make (and test) some kind of backup first. I didn't bother because: (a) there's nothing important in it right now; and (b) YOLO.
</div>

```
helm --namespace gitea uninstall gitea
```

### Create a TLS secret

```
./certs create-cert \
    --issuer-cert k3s-ca.crt --issuer-key k3s-ca.key \
    --out-cert gitea.crt --out-key gitea.key \
    --template server \
    --subject '/CN=git.k3s.differentpla.net'
```

Note that the `CN=` needs to match the server name in DNS. For Gitea, I used `git.k3s...`, rather than `gitea.k3s...`.

```
echo "tls.crt: $(base64 -w0 < gitea.crt)"
echo "tls.key: $(base64 -w0 < gitea.key)"
```

### tls-secret.yaml

```yaml
apiVersion: v1
kind: Secret
type: kubernetes.io/tls
metadata:
  name: gitea-tls
  namespace: gitea
data:
  tls.crt: LS0tLS1...
  tls.key: LS0tLS1...
```

### Edit values.yaml

Edit `values.yaml` so that it looks like this:

```yaml
...
gitea:
  config:
    server:
      DOMAIN: git.k3s.differentpla.net
      PROTOCOL: https
      CERT_FILE: /certs/tls.crt
      KEY_FILE: /certs/tls.key
...
extraVolumes:
- name: gitea-tls
  secret:
    secretName: gitea-tls
extraVolumeMounts:
- name: gitea-tls
  readOnly: true
  mountPath: /certs
```

The `gitea.config` values will be written to `app.ini`. The `extraVolumes` and `extraVolumeMounts` entries are for the
TLS secret.

### Reinstall

```
helm install gitea gitea-charts/gitea --namespace gitea --create-namespace --values values.yaml
```

### loadbalancer.yaml

```yaml
apiVersion: v1
kind: Service
metadata:
  labels:
    app: gitea
  name: gitea
  namespace: gitea
spec:
  type: LoadBalancer
  ports:
  - name: gitea-https   # <--
    port: 443           # <--
    protocol: TCP
    targetPort: 3000
  - name: gitea-ssh
    port: 22
    protocol: TCP
    targetPort: 22
  selector:
    app: gitea
```
