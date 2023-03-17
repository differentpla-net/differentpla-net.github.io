---
title: "Exporting variables from scripts"
date: 2023-03-17T10:08:00Z
---

Often, when you're doing something in an interactive shell, you'd like to set an environment variable from a script. But
you can't, because scripts run as their own process, which means that they can't set environment variables in their
parent (the shell). How do we get around that?

## Motivation

The one that's on my mind right now is that every company I've worked for recently has a script to log into their AWS
cloud, which results in setting the `AWS_SESSION_TOKEN`, `AWS_ACCESS_KEY_ID`, etc., environment variables.

Another example is where a script starts a background process, but needs to record its PID in the environment, so that
other programs can access it. `ssh-agent` is an example of this; it exports the `SSH_AUTH_SOCK` and `SSH_AGENT_PID`
environment variables.

Yet another example is something like `rvm` or `nvm`. `nvm`, for example, manipulates `$PATH` to add or update an  entry that points to `$NVM_VERSION_DIR/bin`.

## Using `eval`

The way that `ssh-agent` does it is by requiring the user to run it using `eval`, as follows:

```sh
eval `ssh-agent`
```

This causes the shell to run the command in backticks -- or `$(...)` if you'd prefer -- and then to evaluate the output as if it was commands entered at the prompt.

Running `ssh-agent` without the `eval` gives you something like the following:

```
SSH_AUTH_SOCK=/var/some-gibberish/ssh-whatever/agent.92365; export SSH_AUTH_SOCK;
SSH_AGENT_PID=92365; export SSH_AGENT_PID;
echo Agent pid 92365;
```

...which, when the shell executes them, will set the two environment variables and output a message.

Another example of using `eval` is the way that, say, `direnv hook zsh` (or `direnv hook bash`) or `kubectl completion
zsh` work: as with `ssh-agent`, they output shell commands that set up various functions, aliases, hooks and so on.

## Functions

Functions can also set environment variables. For example:

```zsh
% hello() { HELLO=$1 }
% hello joe
% echo $HELLO
joe
% hello robert
% echo $HELLO
robert
```

<div class="callout callout-info" markdown="span">
The names are from [Erlang: The Movie](https://www.youtube.com/watch?v=xrIjfIjssLE&t=512s), in case you were wondering.
</div>

## Using `source`

To get our shell to load those functions, we should use the `source` command. Where `eval` runs the command and
evaluates the output, `source` just evaluates the file directly.

This is the mechanism that, for example, `nvm` uses. You add the following to your profile:

```sh
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
```

Note that this uses `.` as a shorthand for `source`, and it escapes it with `\`, which prevents it from being treated as
an alias.

So we can create a script, for example:

```sh
# scripts/hello.sh
hello() { HELLO=$1 }
```

...and then `source` it in our `.bashrc`, for example:

```sh
[ -s "$HOME/scripts/hello.sh" && source "$HOME/scripts/hello.sh" ]
```

## Detecting whether we're being run directly

It can sometimes be useful to be able to tell the difference between being `source`-ed or `eval`-ed. For example, you
might want the `hello` command to print help, or do something actually useful if it's run directly.

For details, start with this Stack Overflow question:
<https://stackoverflow.com/questions/2683279/how-to-detect-if-a-script-is-being-sourced>.
