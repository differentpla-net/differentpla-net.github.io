---
title: "Docker Registry Redux"
date: 2021-12-21T11:57:00Z
tags: raspberry-pi docker k3s
layout: series
series: k3s
---

Wherein I finally bring together all we've learned so far and stand this thing up properly.

## Create a namespace

I'd like to run it in a separate namespace:

```
$ kubectl create namespace docker-registry
```

## Create certificates

Rather than [use an insecure registry]({% post_url 2021/2021-12-16-k3s-docker-registry-insecure %}), we should create a TLS certificate. Because OpenSSL sucks, I'm using [an Elixir script]({% post_url 2021/2021-12-21-elixir-certs %}) that uses the 'x509' library.

Read that post for instructions for creating the root CA and installing it. Or use OpenSSL. You do you.

## Restart services

Once you've installed the root certificate, you need to restart the docker service:

```
sudo systemctl restart docker
```

On the control node, restart `k3s`:

```
sudo systemctl restart k3s
```

On the worker nodes, restart `k3s-agent`:

```
sudo systemctl restart k3s-agent
```

### Create server keypair and certificate

```
./certs create-cert \
    --issuer-cert k3s-ca.crt --issuer-key k3s-ca.key \
    --out-cert docker-registry.crt --out-key docker-registry.key \
    --template server \
    --subject '/CN=docker.k3s.differentpla.net'
```

### Create k8s secret

```
kubectl --namespace docker-registry create secret tls docker-registry-tls \
  --cert=docker-registry.crt --key=docker-registry.key
```

### Inspecting k8s secrets

Kubernetes secrets aren't really secret. You can inspect them. For example:

```bash
{% raw %}kubectl --namespace docker-registry \
    get secret docker-registry-tls \
      --template="{{index .data \"tls.key\" | base64decode}}"
kubectl --namespace docker-registry \
    get secret docker-registry-tls \
      --template="{{index .data \"tls.crt\" | base64decode}}"{% endraw %}
```

## Create the deployment

Create a file, `deployment.yaml`:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: docker-registry
  namespace: docker-registry
  labels:
    app: docker-registry
spec:
  replicas: 1
  selector:
    matchLabels:
      app: docker-registry
  template:
    metadata:
      labels:
        app: docker-registry
        name: docker-registry
    spec:
      containers:
      - name: registry
        image: registry:2
        env:
        - name: REGISTRY_HTTP_TLS_KEY
          value: /secrets/tls.key
        - name: REGISTRY_HTTP_TLS_CERTIFICATE
          value: /secrets/tls.crt
        ports:
        - containerPort: 5000
        volumeMounts:
        - name: docker-registry-vol
          mountPath: /var/lib/registry
        - name: docker-registry-tls
          mountPath: /secrets
      volumes:
      - name: docker-registry-vol
        persistentVolumeClaim:
          claimName: docker-registry-pvc
      - name: docker-registry-tls
        secret:
          secretName: docker-registry-tls
```

## Create a persistent volume claim

We need some storage, so create a file `pvc.yaml`. It uses Longhorn, which [we installed previously]({% post_url 2021/2021-12-21-installing-longhorn %}).

```yaml
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: docker-registry-pvc
  namespace: docker-registry
spec:
  storageClassName: longhorn
  accessModes:
  - ReadWriteOnce
  resources:
    requests:
      storage: 1Gi
```

## Create a load balancer

We could probably get away with using an Ingress for this, but we've [installed MetalLB]({% post_url 2021/2021-12-20-installing-metallb %}), so we'll use that.

```yaml
apiVersion: v1
kind: Service
metadata:
  labels:
    app: docker-registry
  name: docker-registry
  namespace: docker-registry
spec:
  type: LoadBalancer
  ports:
  - port: 443
    protocol: TCP
    targetPort: 5000
  selector:
    app: docker-registry
```

## Apply

```
$ kubectl apply -f deployment.yaml -f pvc.yaml -f lb.yaml

$ kubectl --namespace docker-registry get services
NAME              TYPE           CLUSTER-IP     EXTERNAL-IP     PORT(S)          AGE
docker-registry   LoadBalancer   10.43.125.31   192.168.28.12   443:31355/TCP   3h13m
```

## Configuring DNS

My router uses dnsmasq by default, and we [can add extra entries](https://superuser.com/questions/1646486/how-do-i-create-manual-dns-entries-for-synologys-routers-running-srm) by using the `--addn-hosts=<file>` option.

```
ssh root@192.168.28.1   # same password as 'admin'
```

Create a file, `/etc/dhcpd/addn-hosts`, containing the following:

```
192.168.28.11   nginx.k3s.differentpla.net
192.168.28.12   docker.k3s.differentpla.net
```

The `nginx` entry is our test from installing MetalLB; the `docker` entry is from above.

Create a file, `/etc/dhcpd/dhcpd-addn-hosts.conf`, containing the following:

```
addn-hosts=/etc/dhcpd/addn-hosts
```

Create a file, `/etc/dhcpd/dhcpd-addn-hosts.info`, containing the following:

```
enable="yes"
```

Restart the DHCP server:

```
/etc/rc.network nat-restart-dhcp
```

### Caveats

- It only balances TCP traffic, not ICMP. So `ping` doesn't work.
- dnsmasq doesn't reload the additional hosts automatically. See [this Server Fault question](https://serverfault.com/questions/723292/dnsmasq-doesnt-automatically-reload-when-entry-is-added-to-etc-hosts), for example. This is [by design](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=798653#35).
- I'm not entirely happy about manually editing files (and restarting services) on the router. It seems fragile. It also means that I ought to notify everyone else in the house about potential downtime.
- At some point, I'll add another DNS server (or co-opt CoreDNS?), and set up dnsmasq on the router to forward the subdomain to it.

## Does it work?

```
$ curl https://docker.k3s.differentpla.net/v2/_catalog
{"repositories":[]}
```

```
$ docker pull hello-world
$ docker tag hello-world docker.k3s.differentpla.net/hello-world
$ docker push docker.k3s.differentpla.net/hello-world
$ docker pull docker.k3s.differentpla.net/hello-world
```

```
$ kubectl create deployment hello-world --image=docker.k3s.differentpla.net/hello-world
```

## References

- <https://kubernetes.io/docs/concepts/configuration/secret/>
- <https://docs.docker.com/registry/deploying/>
- <https://kubernetes.io/docs/tasks/inject-data-application/define-environment-variable-container/>
