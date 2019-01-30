TODO: There's an older version of this post on here somewhere.

Once you've installed your Erlang versions with kerl, you'll want to switch
between them.

You can do as kerl says:

    . $HOME/.kerl/erlangs/OTP-21.2.4/activate

...or you can use `direnv`.

In the relevant project directory, create a .envrc file containing the
following:

    use erlang OTP-21.2.4

This assumes that you have a function named `use_erlang` defined in
`~/.direnvrc`. Here's mine:

```
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
