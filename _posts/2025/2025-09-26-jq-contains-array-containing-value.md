---
title: "jq: array contains array containing key with value?"
date: 2025-09-26T13:55
tags: jq
---

In one of our Kubernetes clusters, there are a number of pods failing with "ImagePullBackOff". I want to find the nodes on which these pods are running.

I could just do something like this:

```sh
kubectl get pods -A -o wide | grep ImagePullBackOff | awk '{print $8}' | sort | uniq
```

But I wanted to use `jq`.

In the end, I came up with either of the following:

```sh
kubectl get pods -A -o json | jq '.items[] | select((.status.containerStatuses[] | select(.state.waiting.reason == "ImagePullBackOff"))) | .spec.nodeName'
```

I think I prefer this one using `any(_;_)`, though:

```sh
kubectl get pods -A -o json | jq '.items[] | select(any(.status.containerStatuses[]; .state.waiting.reason == "ImagePullBackOff")) | .spec.nodeName'
```

That is: inside `items[]`, look at `status.containerStatuses[]`. Do any of them have `.state.waiting.reason` set to `"ImagePullBackOff"`?

For non-K8s people out there, the JSON looks like this (severely trimmed):

```json
{
    "items": [
        {
            "spec": {
                "nodeName"
            },
            "status": {
                "containerStatuses": [
                    {
                        "state": {
                            "waiting": {
                                "reason": "ImagePullBackOff"
                            }
                        }
                    }
                ]
            }
        }
    ]
}
```
