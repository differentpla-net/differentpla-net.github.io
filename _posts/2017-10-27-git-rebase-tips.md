---
title: git rebase tips
date: 2017-10-27 13:10+0000
---

*This is an internal email I wrote almost exactly 2 years ago. I had to refer
to it today, and I figured that it bears sharing publicly.*

The use of git rebase came up in the backend retrospective yesterday, and I
said I'd follow up. Here are some random notes and links.

**tl;dr: learn to use `git rebase -i`; `git add -p` is your friend.**

`git rebase` works by replaying the commits from one branch on top of another
commit (usually a branch). This means that if you've been hacking on a feature
branch for a while, you can pretend that you did all of your work on master.

So, I might:

    git checkout rl-cool-feature
    git rebase master
    # fix conflicts, run tests, etc.
    git checkout master
    git merge --no-ff rl-cool-feature
    git push

This allows us to have nice, neat [bow-shaped
branches](http://pdh11.blogspot.co.uk/2013/06/bow-shaped-branches-git-workflow.html).

However, you sometimes run into problems in that "fix conflicts" step, which
can sometimes get painful. At this point a couple of tricks come in useful.

The first is that you don't have to rebase onto a branch. You can rebase onto a
particular commit. This allows you to "ratchet" your branch closer to where you
want it. It doesn't necessarily avoid fixing conflicts, but it make it easier
to find out where the conflict is coming from.

It also feels like you're doing something useful, and it makes `git rebase
--abort` seem like less of a big deal.

This is where interactive rebase comes in. You can use it to edit commit
messages; you can merge commits (squash or fixup); you can reorder commits and
you can flat out delete commits.

There's a good tutorial covering this here: [Git Interactive Rebase, Squash,
Amend and Other Ways of Rewriting
History](https://robots.thoughtbot.com/git-interactive-rebase-squash-amend-rewriting-history),
which covers more or less the same ground as the Pro Git book, in [7.6 Git
Tools - Rewriting
History](https://git-scm.com/book/en/v2/Git-Tools-Rewriting-History).

The other really powerful thing you can do with interactive rebase is to
**split** commits. The book hints at this
[here](https://git-scm.com/book/en/v2/Git-Tools-Rewriting-History#_splitting_a_commit),
but glosses over it by merely mentioning `git reset HEAD^`.

The deal with this is that, in interactive rebase, when you mark a commit as
"edit", git stops replaying commits immediately after the point that commit was
applied.

This means that you can (a) sneak a new commit in (b) add some files and use
`git commit --amend` to include them or (c) actually edit the files in the
commit. To do this, you need to reset the commit, which unstages your changes.
This is what `git reset HEAD^` (or `HEAD~`) does.

At this point, you can break the commit up:

    git reset HEAD~
    # 'foo' and 'bar' are unstaged; you want to put them in separate commits.

    git add foo
    git commit -m "Add foo"
    git add bar
    git commit -m "Add bar"
    git rebase --continue

But, more than that, remember `git add -p`. This **allows you to stage only part
of a file**. That's really powerful; it means that you can use interactive rebase
to split the changes to a particular file across multiple commits.

And that might make your rebase less conflict-prone, because you can deal with
the conflicts in smaller pieces.

