---
title: A bug in libgnutls
date: 2017-10-27 09:06+0000
layout: series
series: libgnutls-bug
---

## Background

An [Electric Imp](https://electricimp.com/) customer reported that, after a recent server deploy, their agent could no longer connect to a Google API.

We could reproduce it with the following agent code:

    local req = http.get("https://sheets.googleapis.com/$discovery/rest?version=v4");
    req.setvalidation(VALIDATE_USING_SYSTEM_CA_CERTS);
    req.sendasync(function(res) {
        server.log(res.statuscode);  // should be 200; was 60
    });

In an Electric Imp agent, HTTP status codes < 100 are curl error codes; error 60 is `CURLE_SSL_CACERT`.

It was possible to work around the problem by turning off certificate validation. Obviously this is not ideal.

## Investigation

- We _couldn't_ reproduce the problem in our staging environment. This is good, because it gives us a working platform to compare with.
- I _could_ reproduce the problem on my development PC. This is good, because it means it's quicker to experiment with tests and fixes.
- I _couldn't_ reproduce the problem using `curl` or `openssl s_client` or `gnutls-cli`.

Because this had something to do with server certificate verification, I guessed that it had something to do with the root CA store, which is stored in `/etc/ssl/certs/` on Ubuntu.

This is managed by the `ca-certificates` package on Ubuntu.

Looking at the installed version of this package on the working versus non-working servers (including my PC) showed that a recent Ubuntu update had made some changes to the root certificates and, presumably, broken something.

That is: the (working) server in staging was using an older version of the package, compared to the (broken) server in production (and my development PC).

We pre-load the CA trust list in our code (it's expensive to parse all those certificates on every outbound agent connection). Fortunately, there's a single PEM file with all of the certificates in: `/etc/ssl/certs/ca-certificates.crt`, which makes it easy to try alternative CA bundles when testing.

I grabbed the (working) `ca-certificates.crt` file from the staging server and confirmed that an agent running on my PC worked. Then I double-checked that with the system-installed file, it didn't work.

I found the [relevant change](https://launchpad.net/ubuntu/+source/ca-certificates/20170717~14.04.1) to the `ca-certificates` package, and what seemed to be the [corresponding upstream Debian bug](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=858064).

Comparing the two files revealed that several certificates had been removed from the CA bundle, *including* the root CA used by `googleapis.com`. This shouldn't have been a problem, because one of the intermediate CA certificates that Google are using _is_ in the CA bundle.

But: other clients (`curl`, etc.) were still working, just not ours.

At this point, while I got on with putting together an [MCVE](https://stackoverflow.com/help/mcve), I posted a [question on security.stackexchange.com](https://security.stackexchange.com/questions/171983/server-certificate-terminates-in-removed-ca-certificate-still-works), where Steffen Ulrich pointed out that there had been a bug in OpenSSL where it was ignoring trusted intermediate CA certificates and then failing because it couldn't find the root CA.

This _exactly_ describes the problem that we were seeing.

Except that we use GnuTLS for our certificate handling, not OpenSSL. For agents, anyway.

## MVCE

Today, I managed to put together a short-ish piece of C++, cobbled together from the GnuTLS examples, which reproduces the problem -- which I posted with a [question to StackOverflow](https://stackoverflow.com/questions/46935436/gnutls-doesnt-correctly-verify-certs-for-googleapis-com). I won't bother reproducing it here.

A little while later, after a bit of back-and-forth in comments, Steffen replied saying that he'd found a corresponding [bug report](https://bugs.launchpad.net/ubuntu/+source/gnutls28/+bug/1722411) against libgnutls28 on Ubuntu 14.04.

That bug report includes a patch.

[Here's how to apply it](http://blog.differentpla.net/blog/2017/10/27/libgnutls-patch).
