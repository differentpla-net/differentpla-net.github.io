---
title: "Resetting the terminal after running rebar3"
date: 2023-03-08T10:12:00Z
tags: erlang rebar3 zsh
---

Sometimes, when you run `rebar3 shell` and you press Ctrl+C twice to quit, your terminal is messed up. This is how I
fixed it in zsh.

Define a function as follows. I've got it in my `.zshrc`:

```
rebar3 () {
	set -o localoptions -o localtraps
	trap '/bin/stty sane' INT
	command rebar3 "$@"
}
```
