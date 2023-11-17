---
title: "Using git with multiple identities"
date: 2023-11-17T10:23:37Z
tags: git
---

I want to keep my work and personal Github accounts separate. Here's how I set that up:

## Background

I've had a personal Github account -- [@rlipscombe](https://github.com/rlipscombe/) -- since 2010. In the past I've just
added my work email address to my account, and made sure I've [configured my email address properly]({% post_url 2020/2020-01-24-git-emails %}), for correct attribution.

This time, my work account is going to be managed by Corporate IT, and I don't feel comfortable blurring that boundary,
so I need to set up a separate Github account.

Because I still want to be able to access both my personal and work accounts from my work laptop, I need to set up git
to manage multiple identities.

## Create another SSH Key

Github doesn't let you attach the same SSH public key to multiple accounts, so you'll need to have one keypair per identity.

```sh
ssh-keygen -t ed25519 -f ~/.ssh/id_work -C "$USER@$(hostname)-Work"
```

For more information: [Generating a new SSH key and adding it to the ssh-agent](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent).

Then you'll need to add it to the new account. See [the docs](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account).

My company requires that the SSH key be authorized for SSO, which is something I've never seen before, but it's just a
couple of clicks, so that's easy enough.

## Using the alternative SSH key

To use the new (non-default) SSH key:

```sh
GIT_SSH_COMMAND='ssh -i ~/.ssh/id_work -o IdentitiesOnly=yes' git clone git@github.com:work-org/some-repo.git
```

I can see this getting very annoying very quickly.

I did some poking around on the Internet, and I can identify a few options, with varying effectiveness.

### Use a separate SSH configuration, with a made up host name

<div class="callout callout-warning" markdown="span">
I don't like this option.
</div>

Put something like this in your `~/.ssh/config` file:

```ssh-config
Host work.github.com
    Hostname github.com
    IdentityFile ~/.ssh/id_work
    IdentitiesOnly yes
```

Then this will work:

```sh
git clone git@work.github.com:big-company/boring-crud.git
```

...and you can continue using your default identity otherwise. If you have multiple non-default identities, you can add
extra prefixes:

```
# ...

Host personal.github.com
    Hostname github.com
    IdentityFile ~/.ssh/id_personal
    IdentitiesOnly yes
```

```sh
git clone git@personal.github.com:you/exciting-side-project.git
```

That's still a bit annoying, though, because you need to remember to add `work.` or `personal.` prefixes to select
between your two identities.

If you're using the `Code` dropdown button in the Github UI, you have to remember to edit it every time. If someone sends you a repo URL in Slack, you have to remember to edit it, etc., etc.

### Using `includeIf`

<div class="callout callout-info" markdown="span">
This is my preferred option.
</div>

The idea here is that your `~/.gitconfig` looks like this (the trailing slashes on the paths are required):

```git-config
[includeIf "gitdir:~/Source/Work/"]
	path = ~/Source/Work/.gitconfig
[includeIf "gitdir:~/Source/Personal/"]
	path = ~/Source/Personal/.gitconfig
```

And then you have `~/Source/Work/.gitconfig` that looks like this:

```git-config
[core]
    sshCommand = ssh -i ~/.ssh/id_work -o IdentitiesOnly=yes
```

...and so on, for each of your identities. It means that you need to keep your work-related and personal projects in
separate directories (until now, I just stuck everything in `~/Source`).

### Other rejected options

- Using something like `direnv` to switch out the `GIT_SSH_COMMAND` environment variable when you change directories.
  - This would also work for the `GIT_AUTHOR_EMAIL`, `GIT_COMMITTER_EMAIL`, etc. variables.
  - I rejected this because it would require all of my `.envrc` files to remember to have `source_up` directives in
    them, and forgetting it just once would break things in weird ways.
- Replacing `git clone` with something that set the environment variable appropriately, and then set `core.sshCommand`
  in the newly-checked-out `.git/config` file.
  - git won't let you replace built-in commands with aliases, so I'd have to use a shell alias or function.
  - I rejected this because I thought it might be fragile.
- I'm already using <https://github.com/DrVanScott/git-clone-init>, but that only works to patch the configuration
  _after_ you've cloned the repo.
