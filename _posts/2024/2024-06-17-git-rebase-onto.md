---
title: "Moving a PR to a different branch"
date: 2024-06-17T08:00:00Z
tags: git github
---

Because I mess it up every time, here's how to move a Github pull request from one branch to another.

If you've got a branch `bug-fix`, on `master`, and you want to move _just those commits_ to `maint`, you need to do the
following:

```sh
git checkout bug-fix                    # the branch containing the changes
git rebase --onto maint bug-fix^        # ^ means a single commit
git push -f                             # force push
```

I'm assuming that if you want to move more than one commit, you'll need to include those in your `bug-fix^`, e.g. with
`bug-fix^^^` or an actual commit hash (presumably the one _before_ where you want to start from).

Then, in the Github web UI, change the base branch by clicking the _Edit_ button next to the PR title.

Once you've done that, your PR should be based on a different branch, and should include just the relevant changes.

If you mess it up, I have no idea how to fix it. I usually just close the PR, and create a new one with a fresh, cherry-picked branch in the correct place.
