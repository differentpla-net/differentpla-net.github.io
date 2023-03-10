---
title: "Using socat to strip HTTPS"
date: 2023-03-11T11:54:00Z
---

I'm trying to figure out how `kubectl` does its thing. Because it uses HTTPS to talk to the API server, I can't use
Wireshark to look at the traffic. Here's how I used `socat` to snoop on the traffic.

## The plan

The plan is to tell kubectl to talk to socat using HTTP and for socat to forward that to the real API server using
HTTPS.

## Add a new cluster

In order to avoid breaking anything, I added a new cluster to `~/.kube/config` as follows:

```yaml
apiVersion: v1
clusters:
- cluster:
    certificate-authority-data: LS0t...LS0K
    server: https://original-api-server:6443
  name: default
- cluster:
    server: http://localhost:8132
  name: socat
#...
```

## Extracting the CA certificate

So that socat can connect to the API server using HTTPS, we'll need the CA certificate. We can get it from the config file by using `yq`; see <https://mikefarah.gitbook.io/yq/>.

```sh
yq -r < ~/.kube/config \
        '.clusters[] | select(.name == "default") | .cluster.certificate-authority-data' | \
    base64 -d > ca.crt
```

## Extracting the user certificate

I'm using K3s, which uses client certificates to authenticate, so I also need to grab the user key and certificate:

```sh
yq -r < ~/.kube/config \
        '.users[] | select(.name == "default") | .user.client-certificate-data' | \
    base64 -d > user.crt
yq -r < ~/.kube/config \
        '.users[] select(.name == "default") | .user.client-key-data' | \
    base64 -d > user.key
```

## Run socat

```sh
original_api_server="$(
    yq < ~/.kube/config \
        '.clusters[] | select(.name == "default") | .cluster.server' | \
    sed 's,https://,,')"

socat \
    "tcp-listen:8132,reuseaddr,fork" \
    "openssl:${original_api_server},cafile=ca.crt,key=user.key,cert=user.crt"
```

## Run kubectl

```
$ kubectl --cluster socat get pods
NAME                         READY   STATUS    RESTARTS   AGE
nginx-76d6c9b8c-jhbw9        1/1     Running   0          42h
nginx-76d6c9b8c-h7q5k        1/1     Running   0          42h
nginx-76d6c9b8c-jhb6h        1/1     Running   0          42h
```

That works. Can we grab the traffic?

## Running tcpdump

```
$ sudo tcpdump -A -i lo port 8132
tcpdump: verbose output suppressed, use -v[v]... for full protocol decode
listening on lo, link-type EN10MB (Ethernet), snapshot length 262144 bytes
...
23:20:02.104147 IP localhost.38678 > localhost.8132: Flags [P.], seq 1:372, ack 1, win 512, options [nop,nop,TS val 2024223897 ecr 2024223897], length 371
E....l@.@.7.............
(.S..t............
x.4.x.4.GET /api/v1/namespaces/default/pods?limit=500 HTTP/1.1
Host: localhost:8132
User-Agent: kubectl/v1.26.1 (linux/amd64) kubernetes/8f94681
...
```

Well, that certainly looks like unencrypted HTTP. I think we're done here.

## Troubleshooting

`E0310 23:12:06.306704   11768 memcache.go:238] couldn't get current server API group list: the server has asked for the client to provide credentials`

Did you provide the correct client certificate and key on the socat command line?

`2023/03/10 23:06:56 socat[11273] E SSL_connect(): error:0A000086:SSL routines::certificate verify failed`

Did you provide the correct CA certificate on the socat command line?
