---
title: "EUnit examples: 'with'"
short_title: "Using 'with'"
date: 2025-10-16T07:25
tags: erlang eunit
layout: series
series: erlang-eunit-examples
---

In earlier posts, [Passing the result from 'foreach']({% post_url 2023/2023-04-02-eunit-examples-foreach-result %}) and
[Passing the result from 'setup']({% post_url 2023/2023-04-02-eunit-examples-setup-result %}), I showed how to
(awkwardly) pass the result from the setup function to the test functions.

It turns out that `eunit` provides a better way to do this, using `with`. Use it like this:

## `setup`

The example is taken from some in-progress work on my `erl509` library; the exact details don't matter.

```erlang
all_test_() ->
    {setup, fun setup/0, 
        {with, [
            fun subject_key_identifier/1,
            fun authority_key_identifier/1
        ]}}.

setup() ->
    % Create an RSA private key and derive the public key.
    PrivateKey = erl509_private_key:create_rsa(2048),
    PublicKey = erl509_private_key:derive_public_key(PrivateKey),
    PublicKey.

subject_key_identifier(PublicKey) ->
    % ...
    ok.
```

## `foreach`

As mentioned previously, `foreach` requires a list: `[Test | Instantiator]`, so we need to wrap the `with` tuple in
another list:

```erlang
all_test_() ->
    {foreach, fun setup/0, [
        {with, [
            fun subject_key_identifier/1,
            fun authority_key_identifier/1
        ]}]}.
```

## Cleanup?

RSA public keys don't need cleaning up, so I omitted the `Cleanup` element in the test generator.