---
title: "k3s on Raspberry Pi: Private Docker"
short_title: "Private Docker"
date: 2020-02-06T17:52:00
layout: series
series: k3s
tags: k3s raspberry-pi docker
---

**Updated:** _See [k3s on Raspberry Pi: Docker Registry]({% post_url 2021-12-10-k3s-docker-registry %}), where I do it "properly"._

---

In order to run our own apps on the cluster, we're going to need a private docker repository.

## Install docker

```
curl -sSL get.docker.com | sh
sudo usermod -aG docker pi
```

Log out and log back in to reload your group memberships.

```
docker run hello-world
```

## Create a self-signed server cert

I couldn't figure out how to get `containerd` to use HTTP when talking to the private docker repository, so I created a self-signed server certificate instead.

```
openssl req -newkey rsa:4096 \
    -nodes -sha256 \
    -keyout rpi201.key \
    -x509 -days 365 -out rpi201.crt \
    -subj "/C=GB/L=Location/O=Org Name/CN=rpi201"
```

## Run the private repo container

```
docker run \
    -d -p 5000:5000 \
    --restart=always \
    --name registry \
    -v "$(pwd)":/certs \
    -e REGISTRY_HTTP_ADDR=0.0.0.0:5000 \
    -e REGISTRY_HTTP_TLS_CERTIFICATE=/certs/rpi201.crt \
    -e REGISTRY_HTTP_TLS_KEY=/certs/rpi201.key \
    registry:2
```

## Install the cert as trusted

On each node:

```
sudo cp rpi201.crt /usr/local/share/ca-certificates/
sudo update-ca-certificates
```

In order to persuade `containerd` to trust the certificate, you'll need to restart it. You might as well reboot the node:

```
sudo shutdown -r now
```

## Links

- <https://docs.docker.com/registry/insecure/>
- <https://docs.docker.com/registry/deploying/#get-a-certificate>
