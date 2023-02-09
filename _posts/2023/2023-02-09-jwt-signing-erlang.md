---
title: "Signing a JSON Web Token in Erlang"
date: 2023-02-09T16:52:00Z
tags: erlang cryptography jwt
---

About 8 years ago, I wrote a post about [verifying JSON Web Tokens (JWT) in Erlang]({% post_url 2015/2015-04-19-jwt-rs256-erlang %}).
3 years later, I wrote another post about [signing a JWT in bash]({% post_url 2017/2017-10-05-google-pub-sub-bash %}).
This is a post combining the two: here's how you sign a JWT using Erlang.

A JWT needs a header, which is base64url-encoded:

```erlang
Header = base64url:encode(
    jsx:encode(
        #{<<"alg">> => <<"RS256">>,
          <<"typ">> => <<"JWT">>}
    )).
```

Then you need a payload with some claims; the list of required vs. optional claims depends on the particular
implementation. I'll use the initial list from the `jwt.io` debugger.

This is also base64url-encoded.

```erlang
Payload = base64url:encode(
    jsx:encode(
        #{<<"sub">> => <<"1234567890">>,
          <<"name">> => <<"John Doe">>,
          <<"iat">> => 1516239022}
    )).
```

The `iat` (issued at) value is from 2018, in case you were wondering, per `date --date=@1516239022`.

We then combine the two:

```erlang
Message = <<Header/binary, ".", Payload/binary>>.
```

The `RS256` algorithm requires an RSA key, so we'll need [to generate one]({% post_url 2023/2023-02-07-generate-rsa-key-erlang %}):

```erlang
SigningKey = public_key:generate_key({rsa, 2048, 65537}).
```

And then we can sign the token, which needs to be base64url-encoded again:

```erlang
Signature = base64url:encode(
    public_key:sign(Message, sha256, SigningKey)).
```

We append that to the header and payload, and we've got our final JSON Web Token:

```erlang
JWT = <<Header/binary, ".", Payload/binary, ".", Signature/binary>>.
```
