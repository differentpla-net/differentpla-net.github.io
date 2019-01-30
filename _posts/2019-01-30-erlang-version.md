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
