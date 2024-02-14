---
title: "Triggering GitHub Actions from a different repository"
date: 2024-02-14T15:26:00Z
tags: github-actions
---

At work right now, I'm working on something that spans multiple GitHub repositories, and I need to trigger a job in one
repository from a different repository.

## Background

I'm working on a single project, broken across multiple repositories. I've got separate repositories for the network
layer, for the message codecs, and so on. I've got another repository containing various integration tests that
shouldn't be included when the project is included in a production package. At some point, I'm going to add a repository
with a fancy-pants web front-end that'll look good in demos, but _definitely_ should be opt-in for production.

For simplicity, let's assume that I've got two repositories, `the-tests` and `the-app`. `the-tests` depends on
`the-app`. Whenever I make a change to `the-app` (the "source"), I want to run a workflow in `the-tests` (the "target").

## Repository Dispatch

Fortunately, GitHub provides a `repository_dispatch` trigger that can be used to trigger a workflow. You add it to
`the-tests`, like this:

```yaml
# .github/workflows/integration-tests.yml
name: "Integration Tests"

on:
  repository_dispatch:
    types:
      - integration-tests

jobs:
  integration-tests:
    runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4

    - name: Run Integration Tests
      run: ./integration-tests.sh
```

I've shown it in a separate workflow file above, but if you want to use it to trigger an existing workflow, you can add
it to that file along with the others. For example:

```yaml
# .github/workflows/main.yml
on:
  push:
    branches: ["main"]
  pull_request:
    branches: ["main"]
  repository_dispatch:
    types:
      - integration-tests
# ...
```

We'll see what `types:` is used for later.

## Triggering it with curl

You can trigger this workflow by using `curl` with the `dispatches` REST endpoint. For more details, see [Create a
repository dispatch
event](https://docs.github.com/en/rest/repos/repos?apiVersion=2022-11-28#create-a-repository-dispatch-event) in the
GitHub documentation. Here's an example:

```sh
curl -qs \
  --fail-with-body \
  -L \
  -X POST \
  -H "Accept: application/vnd.github+json" \
  -H "Authorization: Bearer $BEARER_TOKEN" \
  -H "X-GitHub-Api-Version: 2022-11-28" \
  https://api.github.com/repos/$TARGET_REPO_OWNER/$TARGET_REPO/dispatches \
  -d '{"event_type":"integration-tests"}'
```

- `$BEARER_TOKEN` is a _Fine-grained Personal Access Token_, of which more below.
- `$TARGET_REPO_OWNER` is the owner of the target repository. This is the GitHub organization or user name.
- `$TARGET_REPO` is the name of the target repository. In this example, this would be `the-tests`.

The GitHub documentation doesn't include the `-q` (don't load the config file), `-s` (no progress reporting) or
`--fail-with-body` options, but I think they're a good idea.

## Personal Access Token

In order to access the `dispatches` endpoint, you'll need a Personal Access Token (PAT) which grants programmatic access
to your repositories. Here's how I generated a fine-grained one:

1. In GitHub, click on your profile picture in the top-right and select "Settings".
2. Click "Developer settings", at the bottom-left of the settings page.
3. Click "Personal access tokens" / "Fine-grained tokens" on the left-hand side.
4. Click "Generate new token".
5. Give it a name. Since it will have fine-grained permissions, give it a sensible name and description reflecting that.
6. Under "Repository access", choose "Only select repositories". In the widget that appears, choose the target
   repository (in our example, this is `tests`). This limits the damage that it can cause.
7. Under "Repository permissions", you'll need to choose "Contents: Read and write", per the documentation
   [here](https://docs.github.com/en/rest/authentication/permissions-required-for-fine-grained-personal-access-tokens?apiVersion=2022-11-28#repository-permissions-for-contents).
8. Click "Generate token".
8. **Important:** you're going to need the content of the secret later (it starts with `github_pat_`), so copy it to a
   scratch text file or somewhere. **Do NOT check it in.**

## Triggering it from the source repository

Create a workflow in `the-app` that includes the curl command from above.

```yaml
# .github/workflows/trigger-integration-tests.yml
name: "Trigger Integration Tests"

# Run this action when there's a push to 'main'.
on:
  push:
    branches: ["main"]

jobs:
  trigger-integration-tests:
    name: "Trigger Integration Tests"
    runs-on: ubuntu-latest

    steps:
      - name: "Trigger Integration Tests"
        run: |
            target_repo=the-tests
            curl -qs \
              --fail-with-body \
              -L \
              -X POST \
              -H "Accept: application/vnd.github+json" \
              -H "Authorization: Bearer ${{ secrets.THE_TESTS_PAT }}" \
              -H "X-GitHub-Api-Version: 2022-11-28" \
              https://api.github.com/repos/$GITHUB_REPOSITORY_OWNER/$target_repo/dispatches \
              -d '{"event_type":"integration-tests"}'
```

Notes:
- I used `$GITHUB_REPOSITORY_OWNER`, which evaluates to the owner of _this_ repository (i.e. `the-app`). This means that
  if both repositories are forked, it'll trigger the tests in the correct fork.
- If you want to use an environment variable in `event_type:`, bear in mind shell quoting rules.
- You'll need to configure the `THE_TESTS_PAT` secret; see below.

## Secrets

Workflows can access secrets using the `${{ secrets.NAME }}` syntax. We'll use this to access the Personal Access Token
that we created above.

1. In the `the-app` repository (the "source"), click on "Settings" at the top.
2. Click on "Secrets and variables" on the left-hand side, under "Security".
3. Click on "Actions".
4. Click on "New repository secret".
5. Give it a name. In the above workflow step, it was called `THE_TESTS_PAT`, so use that.
6. Paste in the `github_pat_...` secret from earlier.
7. Click "Add secret".

## Conclusion

At this point, any push to `main` in `the-app` should trigger the "Run Integration Tests" workflow in `the-tests`. You
can extend so that `the-app`, `the-lib`, `the-other-lib`, etc. all trigger the integration tests when they're pushed to.

## What's missing?

- Triggering `the-tests` for pull requests against dependencies. I'll dig into that later -- it'll require messing with
  dependency versions.
- Triggering the integration tests only if all of the actions in `the-app` succeed. If that's all in a single workflow
  file, it'll get ugly. "Reusable workflows" can probably help here.

## What's annoying?

- If you have multiple dependencies, you need to add the triggering workflow to each of them. It might be better to use
  a time-based polling mechanism instead.
- If you push to more than one dependency, it'll trigger multiple times and might not wait until everything's quieted
  down.

## References

- <https://www.amaysim.technology/blog/using-github-actions-to-trigger-actions-across-repos>
- <https://medium.com/hostspaceng/triggering-workflows-in-another-repository-with-github-actions-4f581f8e0ceb>
