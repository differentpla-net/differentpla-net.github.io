---
title: "Using Erlang's logger from escript"
date: 2024-01-05T16:05:00Z
tags: erlang
---

If you're writing an Erlang `escript`, you might want to configure logging. Here's how.

```erlang
main(Args) ->
    % Set the default logging level to 'debug'.
    logger:set_primary_config(level, debug),

    % The 'default' handler is already present.
    % Here's how to make look more like lager.
    logger:set_handler_config(default, #{
        formatter =>
            {logger_formatter, #{
                single_line => true,
                template => [
                    time,
                    " ",
                    "[",
                    level,
                    "]",
                    {pid, [" ", pid, ""], ""},
                    {mfa, [" ", mfa, ":", line], ""},
                    ": ",
                    msg,
                    "\n"
                ]
            }}
    }),
```

