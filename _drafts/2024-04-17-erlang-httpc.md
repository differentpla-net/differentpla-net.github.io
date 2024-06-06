---
title: "Erlang: httpc"
date: 2024-04-16T08:24:00Z
tags: erlang
published: false
---

Start a simple test server:

```sh
python3 -m http.server 8023
```

Start the `inets` application; also `ssl`, otherwise HTTPS links don't work:

```erlang
% Separate calls; support OTP-25.x
{ok, _} = application:ensure_all_started(ssl).
{ok, _} = application:ensure_all_started(inets).
```

Make an HTTP request:

```erlang
{ok, {{_, _Status = 200, _}, Headers, Body}} = httpc:request("http://localhost:8023/").
```

Make an HTTPS request:

```erlang
{ok, {{_, _Status = 200, _}, Headers, Body}} = httpc:request("https://badssl.com/").
```

### Failing HTTPS - expired certificate

In OTP-26.x, connecting to a server with an expired certificate should fail; it will log at notice level.

```erlang
{error, {failed_connect, Why}} = httpc:request("https://expired.badssl.com/").
{inet, _, {tls_alert, {certificate_expired, _}}} = lists:keyfind(inet, 1, Why).
```

In OTP-25.x, SSL verification is disabled by default, so the above will succeed; it will log a warning. To enable
verification, use the following:

```erlang
SslOpts = [{verify, verify_peer}, {cacertfile, "/etc/ssl/cert.pem"}].
{error, {failed_connect, Why}} = httpc:request(get, {"https://expired.badssl.com/", []}, [{ssl, SslOpts}], []).
{inet, _, {tls_alert, {certificate_expired, _}}} = lists:keyfind(inet, 1, Why).
```

(taken from <https://erlef.github.io/security-wg/secure_coding_and_deployment_hardening/inets>)