---
title: "Using 'cert' and 'key' with Erlang/OTP 'ssl'"
date: 2021-02-26T16:16:00Z
tags: erlang
---

Ordinarily when writing an SSL/TLS server or client using Erlang/OTP, you'll use the `certfile` and `keyfile` options, as follows:

```erlang
Port = 5555,
LOpts = [{certfile, "server.crt"}, {keyfile, "server.key"}],
{ok, LSock} = ssl:listen(Port, LOpts).
```

The ['ssl' documentation](https://erlang.org/doc/man/ssl.html#type-common_option) also shows the `cert` and `key` options, which allow you to specify the certificate and key directly, rather than needing to specify filenames.

```erlang
common_option() =
    % ...
    {cert, cert() | [cert()]} |
    % ...
    {key, key()} |
    % ...

cert() = public_key:der_encoded()
key() = {'RSAPrivateKey' | ...,
    public_key:der_encoded()}
```

Note that the ability to specify multiple certificates (i.e. a complete certificate chain) was added in ssl-10.2, in OTP-23.2. My system-installed Erlang/OTP version is 23.0.3, so I wasted some time being confused by that.

I probably shouldn't have been confused, since it was likely [my request](http://erlang.org/pipermail/erlang-questions/2020-May/099487.html) to the Erlang mailing list that resulted in the feature being [added](https://erlang.org/doc/apps/ssl/notes.html#ghlink-improvements-and-new-features-id87971). Thanks, Ingela!.

Anyhow, if -- for whatever reason -- you want to load the certificate and key from files and pass them directly, you need to do something like this:

```erlang
load_certificates(CertFile) ->
    {ok, PemBin} = file:read_file(CertFile),
    Entries = public_key:pem_decode(PemBin),
    [Der || {'Certificate', Der, not_encrypted} <- Entries].

load_key(KeyFile) ->
    {ok, PemBin} = file:read_file(KeyFile),
    [{Type, Der, not_encrypted} | _] = public_key:pem_decode(PemBin),
    {Type, Der}.

listen(Port, Opts0) ->
    Opts = lists:map(
        fun({certfile, CertFile}) -> {cert, load_certificates(CertFile)};
            ({keyfile, KeyFile})) -> {key, load_key(KeyFile)};
            (Opt) -> Opt
        end, Opts0),
    ssl:listen(Port, Opts).
```

Of interest here:
- This example loads multiple certificates from the file; if you're using OTP-23.1.x, you need to load just the first, and return that, rather than a list.
- Only one key is supported, so the sample just grabs the first one.
- It only supports non-encrypted keys. Ordinarily, you can use `{password, KeyPassword}`, but that's only supported with `{keyfile, KeyFile}`.

Would you actually do this in real code? Probably not. The only real reason you might do this is to cache the certificate and key, to avoid going back to disk. You're probably not going to actually need to do that, because:
- Erlang/OTP caches the certificates; see `ssl_pem_cache`.
- Even if it didn't, the OS will cache the file contents.
