---
title: "Building Erlang: Prerequisites (Ubuntu)"
date: 2019-01-30 17:02
layout: series
series: direnv-tool-versions
tags: direnv erlang
---

If you want to use `kerl` to build your Erlang installation, you're going to
need some packages installed first.

For Ubuntu, the list (taken from [here](http://erlang.org/pipermail/erlang-questions/2017-October/093855.html)) is as follows:

This list is correct for Erlang/OTP-24.1 and Ubuntu 20.04 (including on WSL).

## Required

```
sudo apt-get -y install build-essential     # assumed

# These will result in the build failing if they're not present.
sudo apt-get -y install autoconf m4         # ./otp_build: autoconf: not found
sudo apt-get -y install libssl-dev          # No usable OpenSSL found
sudo apt-get -y install libncurses5-dev     # configure: error: No curses library functions found
```

## Recommended

I'd consider this one "essential", because you need it to make observer work.

```
sudo apt-get -y install libwxgtk3.0-gtk3-dev     # wxWidgets not found, wx will NOT be usable
```

Note: If you've got a new enough version of WSL 2, you can install the above, and (e.g.) `observer:start()` will display in a native window.

## Optional

The following may not be correct, because I never bother installing them.

```
# If you want wxWebView:
#   wxWidgets was not compiled with --enable-webview or wxWebView developer package is not installed,
#   wxWebView will NOT be available
sudo apt-get -y install libwxgtk-webview3.0-gtk3-dev

# I consider these "optional", because I've never noticed them missing.
sudo apt-get -y install default-jdk         # jinterface     : No Java compiler found
sudo apt-get -y install unixodbc-dev        # odbc           : ODBC library - link check failed

# These are for the documentation.
sudo apt-get -y install xsltproc fop libxml2-utils
```

_Edited 2021-12-10: Update to OTP-24.x, Ubuntu 20.04 (and WSL)._
