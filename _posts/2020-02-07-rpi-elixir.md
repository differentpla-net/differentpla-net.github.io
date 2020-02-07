---
title: "Installing Erlang and Elixir on Raspberry Pi"
date: 2020-02-07T10:49:06Z
tags: erlang elixir raspberry-pi
---

## Installing Erlang and Elixir

```
wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
sudo dpkg -i erlang-solutions_2.0_all.deb
sudo apt-get update
sudo apt-get install --no-install-recommends esl-erlang elixir
```

## Add hex/rebar support

```
mix local.hex --force
mix local.rebar --force
```

## Install Phoenix

```
mix archive.install --force hex phx_new 1.4.12
```
