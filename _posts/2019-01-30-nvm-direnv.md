---
title: Integrating direnv with nvm
date: 2019-01-30 14:53
---

To integrate `direnv` with `nvm`, add the following to `~/.direnvrc`:

```
use_nodejs() {
    NODE_VERSION="$1"

    type nvm >/dev/null 2>&1 || . ~/.nvm/nvm.sh
    nvm use "$NODE_VERSION"
}
```

Then add (e.g.) the following to your project's `.envrc`:

```
use nodejs 10.15.0
```
