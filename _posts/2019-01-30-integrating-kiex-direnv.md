---
title: Integrating direnv with kiex
date: 2019-01-30 17:07
layout: series
series: direnv-tool-versions
tags: direnv elixir kiex
---

To integrate `direnv` with `kiex`, add the following to `~/.direnvrc`:

```
use_elixir() {
    ELIXIR_VERSION="$1"
    if has kiex; then
        if [ -s "$HOME/.kiex/elixirs/elixir-$ELIXIR_VERSION.env" ]; then
            tput setaf 2
            echo "Using Elixir $ELIXIR_VERSION via kiex"
            tput sgr0
            . "$HOME/.kiex/elixirs/elixir-$ELIXIR_VERSION.env"
        else
            tput setaf 1
            echo "Elixir $ELIXIR_VERSION not available via kiex; using default"
            tput sgr0
        fi
    else
        tput setaf 1
        echo "kiex not available; using default Elixir"
        tput sgr0
    fi
}
```

Then add (e.g.) the following to your project's `.envrc`:

```
use elixir 1.8.1
```
