---
title: "Homebrew: Permission denied @ rb_sysopen"
date: 2022-11-08T10:15:00Z
tags: homebrew macos
---

I'm getting a permission denied error when running `brew search`:

```
% brew search sed
Error: Permission denied @ rb_sysopen - /private/tmp/github_api_headers20221108-22911-hohvak
```

I don't know what's screwing up the permissions on `/private/tmp`, but here's how to fix them:

```
sudo chown root:wheel /private/tmp
sudo chmod 1777 /private/tmp
```
