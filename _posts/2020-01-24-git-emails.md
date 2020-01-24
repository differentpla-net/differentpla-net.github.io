---
title: "Using git with multiple emails"
date: 2020-01-24T10:13:37Z
tags: git
---

Recently I noticed that I'd made some commits to the git repository at work with my personal email address.

I started looking at ways to automatically prevent this.

I found [this blog post](https://collectiveidea.com/blog/archives/2016/04/04/multiple-personalities-in-git).

One of its suggestions is to use [direnv](https://direnv.net/) to configure `GIT_AUTHOR_EMAIL` and `GIT_COMMITTER_EMAIL` for each directory. That won't work for me because I share the repositories with other people, and they don't want that in a shared `.envrc`. I couldn't find a way to get direnv to do its thing without a `.envrc` file, so that's out.

Even if we're not using a shared `.envrc` file, that still needs me to manually add one to the repo, and I want it to be automatic.

Another option I considered was wrapping `git` with my own alias or script that could look at the git remote and set the relevant environment variables. That seemed a bit fragile.

Then I found [this question on Stack Overflow](https://stackoverflow.com/questions/34597186/use-a-different-user-email-and-user-name-for-git-config-based-upon-remote-clone).

The accepted answer uses _conditional includes_, which allows you to specify an
extra configuration file depending on where you've checked stuff out.

```
[includeIf "gitdir:~/work/"]
    path = .gitconfig-work
[includeIf "gitdir:~/play/"]
    path = .gitconfig-play
```

This doesn't work for me because it assumes that you're checking out "work" and "play" to
different directories. That's not a perfect match for what I do.

But: one of the other answers has a link to [https://github.com/DrVanScott/git-clone-init](https://github.com/DrVanScott/git-clone-init). It allows you to match the remote URL against patterns in order to set email and name automatically, and it does it at `git clone` time.

So far, it appears to work perfectly.
