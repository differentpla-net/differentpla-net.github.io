---
title: "Generating a version number from your git repo -- follow up"
date: 2022-11-09T18:46:00Z
tags: git
---

I wrote about [Generating a version number from your git repo]({% post_url 2022/2022-11-05-git-vsn %}) at the weekend. This post contains some follow-up notes and thoughts.

At the end of the previous post, we'd come up with a version number that looked something like this:

```
v1.0.2+2.2ec9c07923.dirty
```

## Environment variable

It's useful to allow overriding the behaviour with an environment variable, for two reasons:

1. We might simply want to lie to it.
2. We might already have run the script, in a parent Makefile for example, and want to avoid doing the work again.

We can do that by putting something like the following near the top of the script.

```bash
if [ -n "$GIT_VSN" ]; then
    echo "$GIT_VSN"
    exit 0
fi
```

## Exporting a tarball

If you're exporting a tarball (with `git archive`), the `.git` directory is gone and you have no tags, so you'll have to
find another way to figure out the version at (later) build time.

One way to do this is to create a marker file containing the version number and put it in the tarball. Then, later, when
you unpack the tarball, the `git-vsn` script can look for that file.

We'll make the `git-vsn` script look for a file named `.git-vsn`. Actually putting the `.git-vsn` file in the tarball is
left as an exercise for the reader.

We want to look up the directory tree until we find the file. This function will help:

```bash
search_up() {
    local look=${PWD%/}

    while [[ -n $look ]]; do
        [[ -e $look/$1 ]] && {
            printf '%s\n' "$look"
            return
        }

        look=${look%/*}
    done

    [[ -e /$1 ]] && echo /
}
```

The function is taken from <https://stackoverflow.com/a/19011599/8446>; we can use it as follows:

```bash
git_vsn_file="$(search_up ".git-vsn")/.git-vsn"
if [ -f "$git_vsn_file" ]; then
    head "$git_vsn_file"
    exit 0
fi
```

## Detecting a work tree

If you're running this inside the git work tree, you probably want to ignore any `.git-vsn` files you find.

You can do this by using `git rev-parse --is-inside-work-tree`; note that it outputs `true` or `false`, which is ugly;
you probably want to redirect that to /dev/null:

```bash
if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    # We're inside the work tree.
```

Also note that only `--is-inside-work-tree` sets the exit code. The other, similar options -- `--is-inside-git-dir` and
`--is-bare-repository` -- don't, for some reason. Since we're not planning on using them, this is only of passing
interest, however.

## Commit count vs '-pre'

Earlier, we awkwardly wedged the commit count into the metadata:

```
v1.0.2+2.2ec9c07923.dirty
#      ^ commit count
```

As mentioned, this could be confusing: someone might only look at the version number, and might ignore the rest. SemVer allows us to put arbitrary suffixes after the version number, but before the `+`. For example, we can call our version `-alpha2`, `-beta`, etc. It even suggests a `-pre` suffix, which seems useful.

Unfortunately, however, we need to put the suffix on the _next_ version number, not the _previous_ one. That is:
`v1.2.0` isn't followed by `v1.2.0-pre`; it should be followed by `v1.3.0-pre` and then eventually by `v1.3.0`.

Here's a potential way to handle that:

```bash
# if there are commits, assume we're working on the next version; increment minor, clear patch and add a -pre suffix.
if [[ "$commits" -gt 0 ]]; then
    ((ver_minor++))
    ver_patch=0

    pre="-pre"
fi
```

It's up for discussion whether we should continue to include the commit count in the metadata. It doesn't help us
identify a particular commit: that's what the SHA is for. It _might_ give us an idea of how much work we've done since
the most-recent tag. I don't have any strong feelings one way or the other, but I opted to discard it.

## Maintenance branches are problematic

If you're doing maintenance branches, then our script, as written so far, gets a bit confused.

Let's say that you've just tagged `v1.2.0` and started work on what will eventually be `v1.3.0`. Commits on the trunk will (correctly) be denoted `v1.3.0-pre`.

But then (oh no), a bug report comes in, and you want to do a maintenance release. So you:

```bash
git checkout v1.2.0   # tag on 'main'.
git checkout -b v1.2  # create maintenance branch
# hack hack hack
# fix fix fix
# test test test
git add hack fix test
git commit -m "Fix: whatever was wrong with it"
git push
git-vsn     # v1.3.0-pre+...
```

Wat? We're on the v1.2 maintenance branch, not the `main` branch that's going to become `v1.3.0`.

If we're on the `v1.2` maintenance branch, shortly after the `v1.2.0` tag, then we probably want the next version to be
`v1.2.1`, and we should denote this build as `v1.2.1-pre`.

The problem we have is that git doesn't _care_ what your branches are called; it doesn't know that your trunk is called
`main` or `master` (or even `trunk`), so it's kinda hard for the `git-vsn` script to work out what you _meant_.

We can _nearly_ solve this by using `git remote show origin`, which will tell us which branch `HEAD` refers to on the
remote. But it requires a connection to the remote; it's slow; and it's not 100% reliable (for reasons that I'm not
going to go into).

Instead, we can just use brute force and ignorance. If the current branch starts with `vX.Y`, then assume we're on a
maintenance branch and use the alternative behaviour:

```bash
re='^v([0-9]+)\.([0-9]+)'
branch=$(git branch --show-current)
if [[ $branch =~ $re ]]; then
    # we're on a maintenance branch
    ((ver_patch++))
else
    # we're on the trunk
    # ...
```

<div class="callout callout-info" markdown="span">
The `git-vsn` script can be found at <https://github.com/rlipscombe/git-vsn>; this blog post refers to the
[v1.3.0 tag](https://github.com/rlipscombe/git-vsn/tree/v1.3.0).
</div>
