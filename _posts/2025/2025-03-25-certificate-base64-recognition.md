---
title: Recognising base64-encoded Certificates
date: 2025-03-25T08:52:00Z
tags: base64
---

If you see a chunk of text beginning with `LS0t`, it's probably a base64-encoded certificate or key. Here's a demonstration.

Previously, I demonstrated how [based64-encoded JSON]({% post_url 2024/2024-11-27-json-base64-recognition %}) always
starts with `eyJ`. Something similar is true for base64-encoded certificates (actually, any base64-encoded PEM format).

PEM formats all look like this:

```
-----BEGIN some-label -----
base64-encoded stuff
-----END some-label -----
```

Since these are multi-line, they're commonly base64-encoded _again_, resulting in something that begins with `LS0t`.
Here's why:

Here's a demonstration:

```sh
% echo -n '-----BEGIN CERTIFICATE' | base64
LS0tLS1CRUdJTiBDRVJUSUZJQ0FURQ==
% echo -n '-----BEGIN PRIVATE KEY' | base64
LS0tLS1CRUdJTiBQUklWQVRFIEtFWQ==
```

Essentially, if you take data in PEM format and base64-encode it, you'll end up with something that starts with
`LS0tLS1CRUdJTiB`. It's enough to be able to recognise the `LS0tLS1` prefix.

Aside: if you just look for the `LS0t` prefix, it might be a YAML file that starts with a document divider; it should be
obvious from context, though.
