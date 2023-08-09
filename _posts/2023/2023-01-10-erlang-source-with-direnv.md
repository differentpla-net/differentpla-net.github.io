---
title: "Building Erlang from source and using it with direnv"
date: 2023-01-10T11:25:00Z
tags: erlang kerl
---

Building and installing Erlang is _almost_ compatible with my `use erlang` [rule for direnv]({% post_url 2019/2019-01-30-integrating-kerl-direnv %}). Here's how to bodge it.

## Lying to kerl

Because the `activate`, `activate.csh`, etc. scripts are expanded from templates during `kerl install`, we can't simply copy them from one of the other Erlang directories.

Instead, we'll fool kerl into thinking that it built Erlang from source, even though we did it.

I'll assume that you've got `$ERL_TOP` still set from when you [built Erlang/OTP]({% post_url 2023/2023-01-09-building-erlang %}).

```sh
grep -q "^git,source" "$HOME/.kerl/otp_builds" || \
    echo "git,source" >> "$HOME/.kerl/otp_builds"

mkdir -p "$HOME/.kerl/builds/source"

[ -e "$HOME/.kerl/builds/source/otp_src_git" ] || ln -sf "$ERL_TOP" "$HOME/.kerl/builds/source/otp_src_git"
```

## Installing Erlang using kerl

```sh
kerl install source "$HOME/.kerl/erlangs/source"
```

Once you've done that, you can put `use erlang source` in `.envrc`.

## Updating the installation

If you're hacking on Erlang, and you rebuild it, you'll need to get kerl to reinstall it. This involves some more trickery:

```sh
kerl delete installation source
kerl install source "$HOME/.kerl/erlangs/source"
```
