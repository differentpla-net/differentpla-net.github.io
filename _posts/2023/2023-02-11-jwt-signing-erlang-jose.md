---
title: "Signing a JSON Web Token in Erlang, using JOSE"
date: 2023-02-11T14:45:00Z
tags: erlang cryptography jwt jose
---

In an earlier post, I showed how to [sign a JSON Web Token in Erlang]({% post_url 2023/2023-02-09-jwt-signing-erlang %}). I used plain-ol' Erlang. Here's how to use the [jose](https://hex.pm/packages/jose) package to do the same thing.

```erlang
SigningKey = public_key:generate_key({rsa, 2048, 65537}).
SigningJWK = jose_jwk:from_key(SigningKey).

JWT = #{<<"sub">> => <<"1234567890">>,
        <<"name">> => <<"John Doe">>,
        <<"iat">> => 1516239022}.
JWS = jose_jwt:sign(SigningJWK, #{<<"alg">> => <<"RS256">>}, JWT).
{_, CompactJWS} = jose_jws:compact(JWS).
```

If you're using the same key, you should see the same output.

One possible complication is that the JSON encoder used ought to be deterministic, but might not be. It doesn't matter
from an integrity point of view, but it makes comparing results impossible.

The JOSE library used here automatically puts `"typ": "JWT"` in the header, which other tools and libraries might not.
