# Displaying git revision in your application

## Background

I'd like my web application to display the git SHA and/or tag that it was
built from.

This will allow me to be sure of which version is running in production
(verifying that my upgrade script ran), and -- if anyone reports a bug -- will
allow me to figure out which version they were using at the time.

## Git SHA

To get the Git SHA, we can do the following:

    git rev-parse --short HEAD

This outputs the 7-character SHA value. You'll get something like the following:

    7810dfc

Note: If you omit the `--short`, you get the long version.

## Local Modifications

If the project is being built on the CI server, then this is enough. However,
we'd like to find out whether there are local modifications:

    git status      # refresh the index
    git diff-index --quiet HEAD || echo '+'

This will echo '+' if there are local modifications, whether they've been
staged or not.

## Committed, but not pushed

### One way

Of course, you could have committed changes that haven't been pushed to origin;
we'd like to know about this as well. There are a couple of ways to do this:

    git rev-list --count HEAD ^origin/master    # assuming 'master' branch.

This assumes that you're on the 'master' branch. To find out the current branch, you can:

    git branch | grep '^\*' | cut -c3-

### Alternatively

Alternatively, you can get the same information from:

    git remote update
    git status --short --branch

This has the (minor) advantage over the previous one that it will also tell you
when your branch is *behind* the origin.

This isn't as useful as it seems, however, because:

 * if you're behind origin with no local modifications, you'll have a valid SHA
   that also exists on origin.
 * if you've got local modifications, `git diff-index` will tell you.
 * if you're both ahead and behind origin (you've committed your local
   changes), `git rev-list` will tell you.

## Branch Name

That was above, but for completeness:

    git branch | grep '^\*' | cut -c3-

Or you can get it from here:

    git status --short --branch

See [my bash
profile](https://github.com/rlipscombe/bash_profile/blob/master/bash_prompt#L41)
for example patterns.

## Tag Name

You can get the tag name by using `git describe` which will output something
like the following:

    v0.3-1-g253d91a

This says that the nearest tag on this branch is v0.3, with 1 local
modificatioqn; it also displays "g" (for "git") and the SHA.

Since we've already dealt with local modifications, etc., you might want to
just use `git describe --exact-match`, which will output (e.g.) `v0.3` if there
are no changes since the tag, or will report an error `fatal: no tag exactly
matches 'SOME SHA'`.

