---
title: Integrating direnv with kerl
date: 2019-01-30 17:03
layout: series
series: direnv-tool-versions
tags: direnv kerl erlang
---

To integrate `direnv` with `kerl`, add the following to `~/.direnvrc`:

```sh
use_erlang() {
    OTP_VERSION="$1"
    if has kerl; then
        OTP_INSTALLATION=$(kerl list installations | grep "^$OTP_VERSION " | cut -d' ' -f2)
        if [ -s "$OTP_INSTALLATION/activate" ] ; then
            tput setaf 2
            echo "Using Erlang/OTP $OTP_VERSION (in $OTP_INSTALLATION) via kerl"
            tput sgr0
            . "$OTP_INSTALLATION/activate"

            export OTP_ROOT="$OTP_INSTALLATION"
            export OTP_VERSION
        else
            tput setaf 1
            echo "Erlang/OTP $OTP_VERSION not available via kerl; using default"
            tput sgr0
        fi
    else
        tput setaf 1
        echo "kerl not available; using default Erlang"
        tput sgr0
    fi
}
```

Then add (e.g.) the following to your project's `.envrc`:

    use erlang OTP-21.2.4

<div class="callout callout-info" markdown="span">
The most up-to-date version of this function is [here](https://github.com/rlipscombe/direnv-tools/blob/main/lib/erlang.sh).
</div>
