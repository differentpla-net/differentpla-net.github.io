## Ingress Controllers

In general:

- https://bryanbende.com/development/2021/05/08/k3s-raspberry-pi-ingress -- good diagram.
- https://levelup.gitconnected.com/a-guide-to-k3s-ingress-using-traefik-with-nodeport-6eb29add0b4b
- https://medium.com/google-cloud/kubernetes-nodeport-vs-loadbalancer-vs-ingress-when-should-i-use-what-922f010849e0

nginx:

- https://platform9.com/blog/building-a-complete-stack-ingress-controllers/
- https://computingforgeeks.com/deploy-nginx-ingress-controller-on-kubernetes-using-helm-chart/
- https://dev.to/sr229/how-to-use-nginx-ingress-controller-in-k3s-2ck2
- https://docs.nginx.com/nginx-ingress-controller/installation/installation-with-helm/
- https://kubernetes.github.io/ingress-nginx/deploy/
- https://medium.com/linux-shots/spin-up-a-lightweight-kubernetes-cluster-on-linux-with-k3s-metallb-and-nginx-ingress-167d98f3583d
- https://minikube.sigs.k8s.io/docs/tutorials/nginx_tcp_udp_ingress/

Why? Because I want to experiment with Erlang clustering, so I need multiple
instances of a service, and I need it to be relatively transparent.

The default k3s-installed Traefik ingress controller only does HTTP and HTTPS
ingress, and it forwards based on host name and path. This is fine, but the
NGINX Ingress controller supports TCP and UDP as well. So I thought I'd play
with that.

Learnings:
- It's really difficult to get them to co-exist. If you're going to use the NGINX Ingress Controller, install k3s without Traefik.
- I got into a state where I installed it with the defaults, and then just _could not_ remove the original port 80/443 settings. So I had to remove everything and start again.
- You can configure helm charts with `--values`.

Apparently Traefk supports TCP anyway. Try that next? https://doc.traefik.io/traefik/v2.0/routing/routers/#configuring-tcp-routers
