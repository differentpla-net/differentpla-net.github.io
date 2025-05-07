---
title: "GitHub Actions containing files"
date: 2025-05-07T07:31Z
tags: github-actions
---

I want to write a GitHub action that I can call from other GitHub workflows, and I want it to contain one or more files
that I can use from the action, specifically a `Dockerfile` (not that it matters). Here's how.

## Actions

To write an GitHub action, you create a repository containing one or more `action.yml` files.

For example, the `actions/checkout@v4` action lives in the `actions` organization, in the `checkout` repository. The
`@v4` refers to a tag. So that's [this file here](https://github.com/actions/checkout/blob/v4/action.yml).

## Composite Actions

The `checkout` action is written in Javascript; I want to write a "composite" action. This is, essentially, a reusable
workflow: the `action.yml` file. The GitHub documentation gives [an example
here](https://docs.github.com/en/actions/sharing-automations/creating-actions/creating-a-composite-action#creating-an-action-metadata-file).

## Including files in an action

When you refer to an action with `uses: Owner/Repo/Path@Ref`, the runner only fetches the `action.yml` file. This makes
it hard to include files (such as a `Dockerfile`) in your action. I think this _might_ be easier with Javascript
actions, but I don't particularly want to write one of those unless I have to.

So, to include files in your action, you need to check out the surrounding repository from _inside_ the action:

```yaml
name: action-with-files
description: "GitHub Action with files"
runs:
  using: "composite"
  steps:
    # check out the extra files we need
    - uses: actions/checkout@v4
      with:
        repository: rlipscombe/action-with-files
        ref: main
        path: action-with-files
    # use the extra files we need
    - run: cat action-with-files/resources/hello.txt
      shell: bash
```

Of note:
- I don't know how to generalise the `ref`; this means that if you tag your actions (as in `uses:
  rlipscombe/action-with-files@v1`), it won't fetch the correct files.
- The `checkout` action cleans out the directory first, meaning that it will delete the caller's working directory. I
  added `path:` to fix that.
- Paths are relative to the top-level working directory. Since I used `path: actions-with-files` and the `hello.txt`
  file is in the `resources` directory in the action, the full path is `action-with-files/resources/hello.txt`.
