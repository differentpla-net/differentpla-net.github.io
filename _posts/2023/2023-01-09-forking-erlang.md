---
title: "Forking Erlang/OTP"
date: 2023-01-09T17:49:00Z
tags: erlang
---

Some notes about hacking on and contributing to Erlang, because I don't do it frequently enough to have all of this in muscle memory.

## Create a fork

1. Use the Github UI to create a fork of `erlang/otp`.
2. Use `git clone` to clone the repository to your machine.
3. Make sure that there's an upstream remote in your clone:

```sh
git remote add upstream git@github.com:erlang/otp.git && \
  git remote set-url --push upstream DISABLED
```

## Create a branch

```sh
git checkout master
git checkout -b cool-thing
```

## Pulling upstream changes

```sh
git fetch upstream
git checkout master && git merge upstream/master && git push
```

## Rebasing your changes back onto master

```sh
git checkout cool-thing
git rebase master
git push -f
```
