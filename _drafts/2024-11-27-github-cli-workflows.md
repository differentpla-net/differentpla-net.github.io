---
title: Using the 'gh' command with workflows
date: 2024-11-27T11:53:00Z
tags: github
---

Today, I are mostly triggering GitHub Workflows. It's getting tedious doing this from the web UI. Here's how to use the
CLI for it.

First, you need to log in:

```
gh auth login
```

To run a particular workflow (`.github/workflows/my-workflow.yaml`), on a particular branch (`my-branch`):

```
gh workflow run my-workflow.yaml --ref my-branch
```

Once the workflow has started, you need to get the run ID:

```
gh run list --workflow=my-workflow.yaml --json databaseId --jq '.[0].databaseId'
```

You can then pass that to `gh run watch` or `gh run view`:

```
gh run watch 1234567890
gh run view 1234567890
```
