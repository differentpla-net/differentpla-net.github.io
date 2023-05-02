---
title: "Installing Tempo"
date: 2023-04-28T13:54:00.000Z
---

I want to play with OpenTelemetry, so I figured I'd install Grafana's Tempo in my cluster. I'm going to use ArgoCD to
install the Helm chart.

## Upgrading ArgoCD

In order to use `values.yaml` with the Helm chart, we need a version of ArgoCD that supports multiple repository sources
for an application (2.6 or newer). I upgraded to the latest stable version:

```sh
wget https://raw.githubusercontent.com/argoproj/argo-cd/stable/manifests/install.yaml -O install.yaml
kubectl --namespace argocd apply -f install.yaml
```

## Creating the ArgoCD application

To use a `values.yaml` file that's _not_ in the original chart repository, you need to add multiple repository sources.
At the moment, this isn't supported by the web front-end or the command-line interface, so I created `application.yaml`
as follows:

```yaml
apiVersion: argoproj.io/v1alpha1
kind: Application
metadata:
  name: tempo
  namespace: argocd
spec:
  project: default
  destination:
    namespace: grafana
    server: https://kubernetes.default.svc
  sources:
    - repoURL: https://grafana.github.io/helm-charts
      chart: tempo-distributed
      targetRevision: 1.2.11
      helm:
        valueFiles:
        - $values/values.yaml
    - repoURL: https://git.k3s.differentpla.net/roger/tempo.git
      targetRevision: main
      ref: values
```

I've already got Grafana installed, so installed Tempo into the `grafana` namespace alongside.

`values.yaml` looks like this (and should be pushed to the repository named in the Application spec):

```yaml
gateway:
  enabled: true
minio:
  enabled: true
traces:
  otlp:
    http:
      enabled: true
    grpc:
      enabled: true
distributor:
  config:
    log_received_spans:
      enabled: true
```

Then I installed it with `kubectl apply`:

```sh
kubectl apply -f application.yaml
```

## Grafana data source

After a few minutes, the installation had completed, so I added it as a data source in Grafana.

1. In Grafana, go to Configuration -> Data sources.
2. Click "Add new data source".
3. Under "Distributed tracing", click "Tempo".
4. Because the Tempo gateway is in the same namespace as Grafana, the URL is just `http://tempo-gateway`.
5. Click "Save & test".

## Generate some traces

```
kubectl --namespace grafana apply -f https://raw.githubusercontent.com/grafana/tempo/main/example/helm/microservices-loadtest.yaml
```

![](/images/2023/2023-04-28-installing-tempo/grafana-tempo-screenshot.png)
