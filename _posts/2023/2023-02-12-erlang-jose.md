```erlang
Plaintext = <<"Hello World">>.
```

```erlang
EncryptionKey = public_key:generate_key({rsa, 2048, 65537}).

rr(public_key).
#'RSAPrivateKey'{modulus = Modulus, publicExponent = PublicExponent} = EncryptionKey.
EncryptionPubKey = #'RSAPublicKey'{modulus = Modulus, publicExponent = PublicExponent}.
```

```erlang
JWK = jose_jwk:from_key(EncryptionKey).
jose_jwk:block_encrypt(Plaintext, JWK).
```

```erlang
dbg:start().
dbg:tracer().
dbg:tp(public_key, '_', []).
dbg:tp(crypto, '_', []).
dbg:p(all, c).
```

```
 call public_key:encrypt_public(<<141,171,231,154,48,63,46,188,74,75,135,5,231,135,79,135>>,{'RSAPublicKey',22960999940703944182956838265544719686397201910696485151082701686216457234146462646746151496302648562884538815129144037390980391930620468665084844248022557115802334972251010562262609554413388093717732091032817711148057359187485435530758346935946187922644513149459260887915634648092413986753260050773437282917783221646640070565121356903908321828855617755128784824772846093118685557137396642910264036467146649383020362615305496752852699868692979327465516984323518203672557032821348497824201587129613821088236019157234939667535531863194858847231325604196000464397016789471932051682398985978524584995732538867140739862127,
                65537},[{rsa_padding,rsa_pkcs1_oaep_padding}])
```

So what we've got there is a short binary, <<141,171,231,154,48,63,46,188,74,75,135,5,231,135,79,135>> -- 16 bytes, being encrypted with the public key.

We must then be doing some other crypto.

Also, where did that binary come from? I assume crypto:strong_rand_bytes.

```
29>
29> jose_jwk:block_encrypt(<<"Hello World">>, JWK).
(<0.1155.0>) call crypto:strong_rand_bytes(16)
(<0.1155.0>) call crypto:strong_rand_bytes(12)
(<0.1155.0>) call public_key:encrypt_public(<<67,201,42,232,137,171,105,110,53,14,127,216,34,141,71,145>>,{'RSAPublicKey',22960999940703944182956838265544719686397201910696485151082701686216457234146462646746151496302648562884538815129144037390980391930620468665084844248022557115802334972251010562262609554413388093717732091032817711148057359187485435530758346935946187922644513149459260887915634648092413986753260050773437282917783221646640070565121356903908321828855617755128784824772846093118685557137396642910264036467146649383020362615305496752852699868692979327465516984323518203672557032821348497824201587129613821088236019157234939667535531863194858847231325604196000464397016789471932051682398985978524584995732538867140739862127,
                65537},[{rsa_padding,rsa_pkcs1_oaep_padding}])
(<0.1155.0>) call crypto:public_encrypt(rsa,<<67,201,42,232,137,171,105,110,53,14,127,216,34,141,71,145>>,[65537,
 22960999940703944182956838265544719686397201910696485151082701686216457234146462646746151496302648562884538815129144037390980391930620468665084844248022557115802334972251010562262609554413388093717732091032817711148057359187485435530758346935946187922644513149459260887915634648092413986753260050773437282917783221646640070565121356903908321828855617755128784824772846093118685557137396642910264036467146649383020362615305496752852699868692979327465516984323518203672557032821348497824201587129613821088236019157234939667535531863194858847231325604196000464397016789471932051682398985978524584995732538867140739862127],[{rsa_padding,rsa_pkcs1_oaep_padding}])
(<0.1155.0>) call crypto:crypto_one_time_aead(aes_gcm,<<67,201,42,232,137,171,105,110,53,14,127,216,34,141,71,145>>,<<25,24,254,183,50,215,17,21,24,196,220,107>>,<<"Hello World">>,<<"eyJhbGciOiJSU0EtT0FFUCIsImVuYyI6IkExMjhHQ00ifQ">>,true)
{#{alg => jose_jwe_alg_rsa,enc => jose_jwe_enc_aes},
 #{<<"ciphertext">> => <<"gDmJRLj4nbQk6Sk">>,
   <<"encrypted_key">> =>
       <<"gzhrO1mnMbTzUzLjhmBbx_i5WcGEFourgzyiDvjIWzKWZPdFFPtWfgrQGoeXRuPdqVGII0Y3_-wcrpGMRFNrrShQI9E9cNpRkH5eDkTS"...>>,
   <<"iv">> => <<"GRj-tzLXERUYxNxr">>,
   <<"protected">> =>
       <<"eyJhbGciOiJSU0EtT0FFUCIsImVuYyI6IkExMjhHQ00ifQ">>,
   <<"tag">> => <<"UQK8VPAxh4Bm2oj7ocfIcw">>}}
```

```erlang
Key = crypto:strong_rand_bytes(16).
EncryptedKey = public_key:encrypt_public(Key, EncryptionPubKey, [{rsa_padding, rsa_pkcs1_oaep_padding}]).
IV = crypto:strong_rand_bytes(12).
AAD = base64url:encode(
        jsx:encode(#{
            <<"alg">> => <<"RSA-OAEP">>,
            <<"enc">> => <<"A128GCM">>
        })).
{Ciphertext, Tag} = crypto:crypto_one_time_aead(aes_gcm, Key, IV, Plaintext, AAD, true).

#{
    <<"ciphertext">> => base64url:encode(Ciphertext),
    <<"encrypted_key">> => base64url:encode(EncryptedKey),
    <<"iv">> => base64url:encode(IV),
    <<"protected">> => AAD,
    <<"tag">> => base64url:encode(Tag)
}.
```


base64url:decode(<<"eyJhbGciOiJSU0ExXzUiLCJraWQiOiJmcm9kby5iYWdnaW5zQGhvYmJpdG9uLmV4YW1wbGUiLCJlbmMiOiJBMTI4Q0JDLUhTMjU2In0">>).
<<"{\"alg\":\"RSA1_5\",\"kid\":\"frodo.baggins@hobbiton.example\",\"enc\":\"A128CBC-HS256\"}">>

jose jwe enc

```erlang
SigningKey = public_key:generate_key({rsa, 2048, 65537}).
JWK = jose_jwk:from_key(SigningKey).

% write that to a file
jose_jwk:to_file("signing.jwk", JWK).

% equivalent to
{_, Bin} = jose_jwk:to_binary(JWK).
ok = file:write_file("signing.jwk", Bin).
```

% echo -n "Hello World" > hello.txt
% jose jws sig -I hello.txt
At least one JWK is required to sign!
% jose jws sig -I hello.txt -k signing.jwk
{"payload":"SGVsbG8gV29ybGQ","protected":"eyJhbGciOiJSUzI1NiJ9","signature":"AXkaStEfn0549OVloD5WIWKr57tq3cOY8Hvin6Zso-pygCLYPaC1WqqJwrJNJf-Gio57oT0-plrjEiAO9RfF1pPuUfn_YGvru-SmZH8mSnYcVzUvk5Y4-Nl-Au4EMLAqfDrzX1CtdYdwRDyobqBa8bzVq55m2NoVYysUt7uHt98gZPDzgHpzlV9vR53J4oQG16kkLzD2b1cwh6jZJnpbaGhDJ04bUfPbWqBmAjowdMO8O4HBBtgwccSRx0LbtnDaMVf5hNQ10Y91o6NtbK-jk7gt7eJYMZla5s0REs6riN9PeO3-GSQqJaeHFVqdHv2t56u5T7T6UiYMXX2ReNzEOw"}
% jose jws sig -I hello.txt -k signing.jwk -c
eyJhbGciOiJSUzI1NiJ9.SGVsbG8gV29ybGQ.AXkaStEfn0549OVloD5WIWKr57tq3cOY8Hvin6Zso-pygCLYPaC1WqqJwrJNJf-Gio57oT0-plrjEiAO9RfF1pPuUfn_YGvru-SmZH8mSnYcVzUvk5Y4-Nl-Au4EMLAqfDrzX1CtdYdwRDyobqBa8bzVq55m2NoVYysUt7uHt98gZPDzgHpzlV9vR53J4oQG16kkLzD2b1cwh6jZJnpbaGhDJ04bUfPbWqBmAjowdMO8O4HBBtgwccSRx0LbtnDaMVf5hNQ10Y91o6NtbK-jk7gt7eJYMZla5s0REs6riN9PeO3-GSQqJaeHFVqdHv2t56u5T7T6UiYMXX2ReNzEOw

Let's compare JWS using my own stuff, JOSE and jose and openssl:

We need a key. We'll use the same key for everything, so that we know it's working:

```erlang
SigningKey = public_key:generate_key({rsa, 2048, 65537}).
```

----

Sign something with jose.

```sh
openssl genrsa -f4 2048 > signing.key
```

It doesn't seem to provide a way to generate a JWK from a PEM-formatted private key, so we need to do that ourselves. There are two ways to do it. We can use `JOSE`:

```erlang
JWK = jose_jwk:from_pem_file("signing.key").
jose_jwk:to_file("signing.jwk", JWK).
```

Then we can sign something with it:

```sh
echo -n "Hello World" > message.txt
% jose jws sig -I message.txt -k signing.jwk -c
eyJhbGciOiJSUzI1NiJ9.SGVsbG8gV29ybGQ.TEVv1TsoJnvLvanbe9_axWfdywidorc5A8BweC5QxkcbPCUmzZyl6Y_PMyVgFCW-h78KSYU-SO9C5cKlLLl3ar4XLFBxSvQEeYwi3rVIp2hDFYecHvSKjkX1FGJeCtOr_c4E6d6dSSpfAvuhDa2DXDXFuno2nZoFiGL7GXAuzNNcykRYnawdyzhKO4Az1s1P_5TW-HVBRTa7imQos60tkLuy5SD53hLaXM_9j4c1vQ8LCE-h3O0ur9nTAlW_gVNejngQTLmFUonGmCRObdVCLP6YSUx0dhWmEY911iSmu_WaMmhWU6hpKe9Q0mUsGLRTxoUY-F5qavpIvdd9sUkMSw
```

Does that match what we'd get if we used `JOSE`?

```erlang
{_, Sig} = jose_jws:compact(jose_jwk:sign(<<"Hello World">>, #{<<"alg">> => <<"RS256">>}, JWK)).
rp(Sig).
<<"eyJhbGciOiJSUzI1NiJ9.SGVsbG8gV29ybGQ.TEVv1TsoJnvLvanbe9_axWfdywidorc5A8BweC5QxkcbPCUmzZyl6Y_PMyVgFCW-h78KSYU-SO9C5cKlLLl3ar4XLFBxSvQEeYwi3rVIp2hDFYecHvSKjkX1FGJeCtOr_c4E6d6dSSpfAvuhDa2DXDXFuno2nZoFiGL7GXAuzNNcykRYnawdyzhKO4Az1s1P_5TW-HVBRTa7imQos60tkLuy5SD53hLaXM_9j4c1vQ8LCE-h3O0ur9nTAlW_gVNejngQTLmFUonGmCRObdVCLP6YSUx0dhWmEY911iSmu_WaMmhWU6hpKe9Q0mUsGLRTxoUY-F5qavpIvdd9sUkMSw">>
```

Yes, it does.




Or we can mess around with Erlang directly (TODO).

Then we can write it to a file:




```sh
```

Let's try it this way


TODO:

- Erlang: AES encryption.
- Erlang: AES decryption.
- Erlang: JWE decryption w/o JOSE.
- Does jose_jwt compact the JWS? Doesn't look like it. How can I get it to compact the JWS before encryption? Am I stuck
  with using the slightly lower-level functions?
