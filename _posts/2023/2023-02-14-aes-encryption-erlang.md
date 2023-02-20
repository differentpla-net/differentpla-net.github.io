---
title: "AES encryption and decryption in Erlang"
date: 2023-02-14T14:14:00Z
tags: erlang cryptography
---

How do I encrypt (and decrypt) things in Erlang, using AES? The simple answer is to use `crypto:crypto_one_time_aead/5,6`. Here's how.

crypto:crypto_one_time_aead()

{Ciphertext, Tag} = (aes_gcm, CEK, IV, SignedJWT, AAD, true).
