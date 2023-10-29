---
title: "Exporting SNMP metrics to Prometheus"
date: 2023-10-29T14:31:00.000Z
---

My DS416play keeps falling off the network, requiring a hard reset. I wondered whether it was possible to monitor it
somehow. It turns out that it responds to SNMP queries, so here's my attempt to export all of that into Grafana.

The plan:
1. Run prom/snmp-exporter in my K3s cluster.
2. Get it talking to the DS416 (and to the DS923+).
3. Build some VictoriaMetrics scrapers.
4. Pretty graphs.

One possible problem here is that SNMP is kinda complicated (you can tell because it has "simple" in the name), and I
have no idea what I'm doing.

## snmp-exporter

Here's the deployment file:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  labels:
    app.kubernetes.io/name: snmp-exporter
  name: snmp-exporter
  namespace: monitoring-system
spec:
  replicas: 1
  selector:
    matchLabels:
      app.kubernetes.io/name: snmp-exporter
  template:
    metadata:
      labels:
        app.kubernetes.io/name: snmp-exporter
    spec:
      containers:
        - name: snmp-exporter
          image: prom/snmp-exporter:v0.24.1
          ports:
            - containerPort: 9116
```

Here's the service:

```yaml
apiVersion: v1
kind: Service
metadata:
  name: snmp-exporter
  namespace: monitoring-system
spec:
  selector:
    app.kubernetes.io/name: snmp-exporter
  type: ClusterIP
  ports:
    - name: snmp-exporter
      port: 9116
      targetPort: 9116
```

It's a `ClusterIP` service because I want it to be accessible from VictoriaMetrics inside the same cluster, but it
doesn't need to be accessible externally.

A quick `kubectl apply -f deployment.yaml -f service.yaml` and the pod is running.

Using `kubectl port-forward -n monitoring-system services/snmp-exporter 9116:9116` and browsing to http://localhost:9116/
shows that it seems to be vaguely working.

## Enabling SNMP service on the Diskstation

On the Synology Diskstation, go to _Control Panel_ / _Terminal & SNMP_. On the _SNMP_ tab, enable the _SNMPv3_ service.
It needs a username and password, so generate something using your password manager and stick that in here. I also
enabled SNMP privacy.

My DS416play has the firewall disabled, so there's nothing to do here.

## Configuring snmp.yml

The [default snmp.yml file](https://github.com/prometheus/snmp_exporter/blob/main/snmp.yml) for `snmp-exporter` has all
kinds of non-Synology-related stuff in it. We'll fix that later. More importantly, it doesn't contain the username and
password we chose above, so we'll need to fix that.

```sh
mkdir -p etc/snmp_exporter/
kubectl --namespace monitoring-system exec -it deployments/snmp-exporter -- \
    cat /etc/snmp_exporter/snmp.yml > etc/snmp_exporter/snmp.yml
```

Edit the top of the file to look like this (use the username and password from earlier):

```yaml
auths:
  snmpv3:
    security_level: authPriv
    auth_protocol: MD5
    username: snmp-exporter
    password: password123
    priv_protocol: DES
    priv_password: password123
    version: 3
# ...etc.
```

I initially tried to use a ConfigMap with kustomize, but the file is just too large. Instead, I'm going to use another
container:

```dockerfile
FROM docker.io/prom/snmp-exporter:v0.24.1
COPY /etc/snmp_exporter/snmp.yml /etc/snmp_exporter/snmp.yml
```

```sh
podman build . -t docker.k3s.differentpla.net/snmp-exporter:v0.24.1-differentpla-net
podman push docker.k3s.differentpla.net/snmp-exporter:v0.24.1-differentpla-net
```

Then we need to edit the deployment YAML to use that image. Ideally we'd put the username and password in a secret, but
I haven't figured out how to do that yet. It might be possible to do it with a startup script, since we're using a
custom container image.

## Try it out

With the new configuration and container, and with the port-forward, I can browse to <http://localhost:9116/>, and enter
the following:

- Target: `the IP address of my DS416`
- Auth: `snmpv3`
- Module: `if_mib`

I had to use the IP address, because the default Pod DNS [doesn't get on with my home
network](https://blog.differentpla.net/blog/2022/02/25/pod-dns-problems/). `snmpv3` comes from the new section in
`snmp.yml`, and `if_mib` is the default.

Pressing the "Submit" button results in a bunch of Prometheus metrics:

```
# HELP ifAdminStatus The desired state of the interface - 1.3.6.1.2.1.2.2.1.7
# TYPE ifAdminStatus gauge
ifAdminStatus{ifAlias="",ifDescr="eth0",ifIndex="3",ifName="eth0"} 1
ifAdminStatus{ifAlias="",ifDescr="eth1",ifIndex="4",ifName="eth1"} 1
ifAdminStatus{ifAlias="",ifDescr="lo",ifIndex="1",ifName="lo"} 1
```

None of these are Diskstation-specific. That's a topic for a later blog post and probably involves MIB files.

## References

- <https://mariushosting.com/monitor-your-synology-with-grafana-and-prometheus-dashboard/>
- <https://github.com/prometheus/snmp_exporter>
- <https://github.com/hakengineer/kubernetes-prometheus-grafana-snmp_exporter/blob/master/snmp-exporter.yml>
