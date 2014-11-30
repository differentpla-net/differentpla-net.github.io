---
title: "sudo: unable to resolve host"
date: 2014-11-30 11:58:18
---

Attempting to `sudo bash`, I was confronted with:

    ubuntu@ip-172-30-0-195:~$ sudo bash
    sudo: unable to resolve host ip-172-30-0-195

This is because the [hostname doesn't
appear](http://askubuntu.com/questions/59458/error-message-when-i-run-sudo-unable-to-resolve-host-none)
in `/etc/hosts`, so edit it as follows:

    127.0.0.1 localhost
    172.30.0.195 ip-172-30-0-195

    # etc.
