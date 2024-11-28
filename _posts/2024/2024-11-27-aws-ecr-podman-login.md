---
title: How does logging into AWS ECR with podman work?
date: 2024-11-27T09:44:47Z
tags: aws ecr podman
---

If you want to use Amazon Elastic Container Registry (ECR) with podman, you'll do something like this: `aws ecr
get-login-password ... | podman login ...`. How does that work?

## `aws ecr get-login-password`

If you run `aws ecr get-login-password`, it will spit out a big chunk of base64-encoded JSON. You can recognise this because [it starts with `eyJ`]({% post_url 2024/2024-11-27-json-base64-recognition %}).

If we pipe that output to `| base64 -d | jq`, we get something like this:

```json
{
  "payload": "qe...hw=",
  "datakey": "AQ..k=",
  "version": "2",
  "type": "DATA_KEY",
  "expiration": 1732743887
}
```

The `payload` and `datakey` values are opaque data. You can see this for the `payload` value:

```sh
% aws ecr get-login-password | base64 -d | jq -r '.payload' | base64 -d | file -
/dev/stdin: data
```

Interestingly, if you do the same for `datakey`, `file` (on macOS, at least) reports "Windows Precompiled iNF". I
suspect that it's mistaken.

The `expiration` value is Unix epoch, in seconds. It's 12 hours from when I ran the command. You can see the expiry by
running the `date` command, as follows:

```
% date -d @1732743887
Wed Nov 27 21:44:47 GMT 2024
```

## `podman login`

The actual content of the "login password" (it's actually a token) is kinda irrelevant. It's just something that we give
to `podman login` (or `docker login`, but that's a subject for another post) for later.

We pipe the output from `aws ecr get-login-password` to `podman login`, as follows:

```sh
aws ecr get-login-password --region eu-west-1 | podman login --username AWS --password-stdin "$ECR_REGISTRY"
```

The important part is the `--password-stdin` which tells `podman login` to read the password from stdin.

<div class="callout callout-info" markdown="span">
This is a security feature: We don't want to pass it on the command line:

```sh
# DON'T DO THIS!
podman login --username AWS --password $(aws ecr get-login-password)
```

If we did that, the password would be visible in the process list (in `ps` or `/proc/PID/cmdline`). Other users might be
able to see it. So the convention is to pass it via stdin.
</div>

The registry is specified as `$AWS_ACCOUNT.dkr.ecr.$AWS_REGION.amazonaws.com`

`podman login` remembers the username ("AWS") and password (the token) for later use of the given registry, when running
`podman push` commands, for example.

By default, it stores the username and password in the `~/.config/containers/auth.json` file, which looks like this
(I've ellided the AWS registry name):

```json
{
  "auths": {
    "[...].amazonaws.com": {
      "auth": "QV...0="
    }
  }
}
```

If we take that value (which is base64-encoded), we get back to our username/password pair from earlier:

```
% cat ~/.config/containers/auth.json | jq -r '.auths[].auth' | base64 -d
AWS:eyJ...
```

```
% cat ~/.config/containers/auth.json | jq -r '.auths[].auth' | base64 -d | cut -d: -f2 | base64 -d | jq
{
  "payload": "qe...
...etc.
```

## Conclusion

All of which is to say: `podman login` is persisted to a file, and you _can_ put it in a separate CI step.