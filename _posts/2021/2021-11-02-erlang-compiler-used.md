---
title: "Which Erlang compiler was used?"
date: 2021-11-02T09:04:00Z
tags: erlang
---

To get the version of the compiler used to compile a particular `.beam` file:

```erlang
Beam = "path/to/foo.beam".
{ok, {_Module, [{compile_info, CInf}]}} = beam_lib:chunks(Beam, [compile_info]).
CVsn = proplists:get_value(version, CInf).
```

## Erlang/OTP version?

However, the compiler is versioned independently from the main Erlang/OTP version. For example, Erlang/OTP 24.0.6 has
compiler version 8.0.2.

If you want to find out which compiler goes with which OTP version, it gets tricky.

The release notes can be found here: <https://www.erlang.org/news/tag/release>. For each of those, find the
corresponding README (for example <https://erlang.org/download/OTP-24.1.README>). That, however, doesn't track
point-releases.

The compiler has its own release notes, here: <http://erlang.org/doc/apps/compiler/notes.html>, which tracks point
releases, but doesn't mention Erlang/OTP versions.

## kerl

If you're using `kerl` to manage your Erlang versions, you can try this:

```bash
# ~/.kerl/erlangs is where I keep my kerl installs.
$ find ~/.kerl/erlangs/ -name 'compiler-*' | sort
/home/roger/.kerl/erlangs/OTP-21.3.7/lib/compiler-7.3.2
/home/roger/.kerl/erlangs/OTP-22.1.1/lib/compiler-7.4.6
/home/roger/.kerl/erlangs/OTP-22.3.3/lib/compiler-7.5.4
/home/roger/.kerl/erlangs/OTP-22.3.4.21/lib/compiler-7.5.4.3
/home/roger/.kerl/erlangs/OTP-23.1.3/lib/compiler-7.6.5
/home/roger/.kerl/erlangs/OTP-23.2.4/lib/compiler-7.6.6
/home/roger/.kerl/erlangs/OTP-23.3.1/lib/compiler-7.6.7
/home/roger/.kerl/erlangs/OTP-23.3.2/lib/compiler-7.6.7
/home/roger/.kerl/erlangs/OTP-24.0.2/lib/compiler-8.0.1
/home/roger/.kerl/erlangs/OTP-24.0.5/lib/compiler-8.0.2
/home/roger/.kerl/erlangs/OTP-24.0.6/lib/compiler-8.0.2
/home/roger/.kerl/erlangs/OTP-24.0/lib/compiler-8.0
/home/roger/.kerl/erlangs/OTP-24.1.2/lib/compiler-8.0.3
/home/roger/.kerl/erlangs/OTP-24.1/lib/compiler-8.0.3
```

## Erlang/OTP repository

If you've got a local `erlang/otp` repo, you can use the following:

```bash
git tag | grep "^OTP-" | while read -r tag ; do vsn="$(git show "${tag}:lib/compiler/vsn.mk")" ; echo "$tag: $vsn" ; done
```

## git blame / git tag

Alternatively, you could use `git blame lib/compiler/doc/src/notes.xml` to see which commit added the change notes. From
there, you can use `git tag --contains <commit>`.
