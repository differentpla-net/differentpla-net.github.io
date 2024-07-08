---
title: Getting Erlang version
date: 2019-01-30 17:05
layout: series
series: direnv-tool-versions
tags: erlang
---

Use this helper function:

```
erlang_version() {
    erl -eval '
        {ok, Version} = file:read_file(
                            filename:join(
                                [code:root_dir(),
                                 "releases",
                                 erlang:system_info(otp_release),
                                 "OTP_VERSION"])),
        io:format("Erlang/OTP ~s", [Version]),
        halt().' -noshell
}
```

<div class="callout callout-warning" markdown="span">
Note that this only works when you run `erl`; it doesn't work in a released application.
</div>
