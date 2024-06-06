---
title: "Erlang Application Configuration"
date: 2024-04-16T08:24:00Z
tags: erlang
---

# Configuration

- Application Environment
    - Configuration file
    - `-App Key Value` command line option.
    - aside: application keys, `application:get_all_key()`.
        - from .app file, environment is in there as well, `env` section. Useful for defaults, instead of specifying them
    in code.
    - copied from .app file into environment when queried, not before.
- Settings are in ETS. Start with `application:get_all_env/1`, follow it to `application_controller:get_all_env/1`,
    and thence to the `ac_tab` ETS table.
- Environment variables
- 12-factor app.
- Easy to manage in k8s deployments.
- Flat
- os:getenv
- aside: Since OTP-something, separate C vars and Erlang vars, which can be annoying.
- Immutable, externally. Stored in the process memory/.
- envy

- [Configuration file](https://www.erlang.org/doc/man/config.html)
- [Applications - Configuring an Application](https://www.erlang.org/doc/design_principles/applications#configuring-an-application)
