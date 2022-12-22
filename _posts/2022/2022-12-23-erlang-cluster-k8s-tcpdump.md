---
title: "Erlang cluster on Kubernetes: tcpdump"
short_title: "tcpdump"
date: 2022-12-22T10:04:00.000Z
layout: series
series: erlang-cluster-k8s
---

TODO: tcpdump

```
kubectl --namespace erlclu debug --quiet -i erlclu-7d86f49786-trrx8 \
    --target=erlclu --image=nicolaka/netshoot -- \
        tcpdump -i eth0 -s 65535 -w - > dump.pcap
```

Do NOT include `-t` in `-it`, and DO include `--quiet`, otherwise various human-readable stuff gets written, which
confuses Wireshark.
