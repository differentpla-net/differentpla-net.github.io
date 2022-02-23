This means that -- if we want TLS termination for the Ingress -- we also need an Ingress for SSH. But: SSH doesn't do host routing, so we can't use the same IP address as everything else _anyway_. Can we point another IP address at a different Ingress, where host routing isn't a thing?

```
$ kubectl --namespace kube-system get service traefik
NAME      TYPE           CLUSTER-IP    EXTERNAL-IP     PORT(S)                      AGE
traefik   LoadBalancer   10.43.50.40   192.168.28.10   80:32034/TCP,443:31470/TCP   42d
```

That LoadBalancer points to the following:

```
  selector:
    app.kubernetes.io/instance: traefik
    app.kubernetes.io/name: traefik
```

...which implies that we can have multiple instances of Traefik...?

You can certainly use separate ingress classes for different ingresses. See https://stackoverflow.com/a/61488750/8446. See also https://blog.baeke.info/2019/07/01/quick-tip-deploying-multiple-traefik-ingresses/.

Worth noting that now I've got MetalLB running, I could install nginx-ingress at the same time as Traefik.

Traefik supports TCP (and UDP) ingress as well as HTTP ingress. I suspect that we should play with that a little bit first.
