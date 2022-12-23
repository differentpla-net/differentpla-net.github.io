---
title: "Erlang cluster on Kubernetes: Polling the CertificateRequest"
short_title: "Polling CertificateRequest"
date: 2022-12-23T17:19:00.000Z
layout: series
series: erlang-cluster-k8s
tags: kubernetes cert-manager
---

In a [previous post]({% post_url 2022/2022-12-20-erlang-cluster-k8s-certificate-requests-cert-manager %}), I used `sleep
5s` to wait for _cert-manager_ to complete the `CertificateRequest`. Instead, we should poll the `status` field.

That looks like this:

```bash
# Give it a chance to complete before we poll it the first time:
sleep 1s

for _ in 1 2 3 4 5; do
    res=$(curl -s \
        --header "Accept: application/json" \
        --header "Authorization: Bearer ${AUTH_TOKEN}" \
        --cacert "${CA_CERT_BUNDLE}" \
        "${certificate_requests_base_url}/$request_name")
    ready_status=$(echo "$res" | jq -r '.status.conditions[] | select(.type == "Ready") | .status')
    if [ "$ready_status" = "True" ]; then break; fi

    sleep 5s
done

if [ "$ready_status" != "True" ]; then exit 1; fi
```

We start with a brief pause, to give _cert-manager_ a chance, so that we don't immediately poll the status and then
sleep for 5s. Then, trying 5 times, we check to see if `Ready` becomes set to `True`. If it does, our certificate is
issued and we can continue. If, after 5 attempts, it doesn't, we've got a problem and we should fail.

This is fairly simplistic. After 5 attempts (~26 seconds), it gives up. If your cluster is taking longer than this to
issue certificates, you might want to extend the sleep, or increase the number of retries. It might even be worth making
them configurable.

It wasn't worth it for me: me cluster issues certificates essentially immediately. There's also an argument to be had
about whether it's better to fail to create a pod (by failing fairly quickly) or to leave the pod pending (by retrying
for a long time).
