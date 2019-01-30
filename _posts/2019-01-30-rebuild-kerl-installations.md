---
title: Rebuilding kerl Installations
date: 2019-01-30 16:50
---

If `kerl list installations` is displaying Erlang installations that you
deleted ages ago, and you've got all of your installations in
`~/.kerl/erlangs/`, you can rebuild the list by running the following command:

    \ls ~/.kerl/erlangs/ | \
        awk -v home=$HOME '{print $1 " " home "/.kerl/erlangs/" $1 }' > ~/.kerl/otp_installations

Note the backslash, which avoids any `ls` alias you might have.
