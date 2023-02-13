---
title: "Adding GitHub authorization to a Cowboy application"
date: 2023-01-25T15:47:00Z
tags: erlang
---

## Create a cowboy app

```
rebar3 new app name=example_rp
```

## Add cowboy dependency

- rebar.config
- src/example_rp.app.src

## Add a listener and handler

Note that this is browser-based, so the URL doesn't need to be externally accessible.

## Create an OAuth App

- <https://docs.github.com/en/developers/apps/building-oauth-apps/creating-an-oauth-app>
  - Application Name: whatever.
  - Homepage URL: whatever.
  - Authorization callback URL: http://localhost:8160/auth/github/callback

- Copy the Client ID
- Create a new Client Secret
  - Copy it

You'll need to keep these safe. Put them in a `.env` file and make sure you don't check it in.

Get direnv to load that file: `dotenv`.

Don't put them in the config file, 'cos that's gonna get checked in. If you're on K8s or whatever, use secrets.

## Useful(?) libraries

The following libraries exist:

- <https://hex.pm/packages/oidcc_cowboy>, but it expects Cowboy 1.x, so that's out.
- <https://github.com/Erlang-Openid/oidcc>, which was last updated in 2022, so that might be useful.
