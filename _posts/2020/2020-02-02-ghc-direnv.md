---
title: "Integrating direnv with ghc"
date: 2020-02-02T15:32:31Z
layout: series
series: direnv-tool-versions
tags: direnv haskell ghc
---

I'm reading "Practical Haskell", which requires a different version of Haskell than
Ubuntu installs so I figured I'd add another entry to my direnv series.

Install the GHC PPA with `sudo add-apt-repository ppa:hvr/ghc`, then install the
relevant version of GHC and Cabal. For "Practical Haskell", this is:

```
sudo apt-get install ghc-8.6.3 cabal-install-2.4
```

Then add the following to `~/.direnvrc`:

```
use_ghc() {
    GHC_VERSION="$1"
    GHC_PREFIX="/opt/ghc/$GHC_VERSION"
    load_prefix "$GHC_PREFIX"
}

use_cabal() {
    CABAL_VERSION="$1"
    CABAL_PREFIX="/opt/cabal/$CABAL_VERSION"
    load_prefix "$CABAL_PREFIX"
}
```

Then add the following to `.envrc`:

```
use ghc 8.6.3
use cabal 2.4
```
