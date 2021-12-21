---
title: "Disabling Klipper"
date: 2021-12-20T20:55:00Z
tags: raspberry-pi
layout: series
series: k3s
---

I forgot to [disable Klipper](https://rancher.com/docs/k3s/latest/en/networking/#disabling-the-service-lb), the K3s-provided load balancer.

Edit the `/etc/systemd/system/k3s.service` file. It looks like this:

```
...
ExecStart=/usr/local/bin/k3s \
    server \

```

Yeah, I don't know what that trailing backslash is doing there.

Edit it so that it looks like this:

```
...
ExecStart=/usr/local/bin/k3s \
    server --disable servicelb \

```

Then restart:

```
sudo systemctl daemon-reload && sudo systemctl restart k3s
```

After a short while (less than a minute), the `svclb-` pods will stop.

## References

- <https://rancher.com/docs/k3s/latest/en/networking/#disabling-the-service-lb>
- <https://metallb.universe.tf/configuration/k3s/>
- <https://www.reddit.com/r/kubernetes/comments/pwmay3/modifying_running_k3s_cluster_configuration/>
