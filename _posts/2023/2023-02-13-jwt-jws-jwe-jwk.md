---
title: "JSON Web Signatures, Encryption, etc."
date: 2023-02-13T12:58:00Z
tags: cryptography jwt
---

I've been spending some time looking at JSON Web Tokens, specifically the JSON Web Signature (JWS) and JSON Web Encryption (JWE) specifications, and it's all been a bit overwhelming and confusing.

I had a realisation that finally made it click:

<div class="callout callout-primary" markdown="span">
All you're doing is taking something and wrapping it. You can take an arbitrary chunk of data and sign it; if you wrap it with the relevant header, that's JWS. If you take a different arbitrary chunk of data and encrypt that, it's JWE -- as long as you attach the relevant header.
</div>

The way it's used for JSON Web Tokens makes you think that it's _all about JWT_. It's not.

When we use it for JWT, all we're doing is taking that JWT and wrapping it in JWS and then wrapping that in JWE.

## References

- [RFC 7515](https://www.rfc-editor.org/rfc/rfc7515.html): JSON Web Signature (JWS)
- [RFC 7516](https://www.rfc-editor.org/rfc/rfc7516.html): JSON Web Encryption (JWE)
- [RFC 7517](https://www.rfc-editor.org/rfc/rfc7517.html): JSON Web Key (JWK)
- [RFC 7518](https://www.rfc-editor.org/rfc/rfc7518.html): JSON Web Algorithms (JWA)
- [RFC 7519](https://www.rfc-editor.org/rfc/rfc7519.html): JSON Web Token (JWT)
