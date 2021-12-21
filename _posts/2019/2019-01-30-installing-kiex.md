---
title: Installing kiex
date: 2019-01-30 17:10
layout: series
series: direnv-tool-versions
tags: elixir kiex
---

```
\curl -sSL https://raw.githubusercontent.com/taylor/kiex/master/install | bash -s
```

Then ensure it's in your path. I copied it to `/usr/local/bin`:

    sudo cp ~/.kiex/bin/kiex /usr/local/bin

Or you could follow the instructions that `install` gives you:

> Add the following to your shell's config file (.bashrc/.zshrc/.cshrc):

    test -s "$HOME/.kiex/scripts/kiex" && source "$HOME/.kiex/scripts/kiex"
