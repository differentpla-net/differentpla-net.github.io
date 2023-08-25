---
title: "Using the GitLab API with 'glab'"
date: 2023-08-25T13:19:00.000Z
tags: gitlab
---

I wanted to get a list of all of the GitLab projects of which I'm a member. Here's how I used `glab` -- GitLab's CLI
tool to do that.

## Log In

```sh
glab auth login
```

It asks whether to log into gitlab.com or a self-hosted instance; I picked `gitlab.com`.

Then it asks whether to use web-based or token-based login. I chose `Token`.

My personal access tokens had all expired, so I went to https://gitlab.com/-/profile/personal_access_tokens and created
a new one. I called it `glab`, accepted the default expiration date (one month) and gave it `api` and `read_repository`
scopes, as recommended by the CLI.

Note: I've managed with just `read_api` in the past, despite the recommendation. I guess it depends on what kinds of
actions you're planning on doing.

Copy the generated token from the web page and paste it into the `glab` prompt. The token is saved to the configuration
file.

For the default git protocol, I chose SSH, since I've already got that set up and working. We'll see if it breaks
anything later.

## Listing projects with `glab repo list`

If you run `glab repo list`, you'll get a list of your repositories (i.e. the ones where you are an owner).

`glab repo list --member` lists the repositories where you are a member.

However, neither of these are particularly useful programmatically -- the output is paginated and displayed in tabular
form.

## Listing projects with `glab api`

Instead, we can use `glab api` as follows:

```sh
glab api --paginate 'projects?membership=true&simple=true'
```

This spits out a bunch of JSON, which can be filtered by using `jq`. I also recommend redirecting it to a file, because
it's a bit slow and by using `--paginate`, we're being kinda spammy with API requests:

```sh
glab api --paginate 'projects?membership=true&simple=true' > gitlab-projects.json
```

Then we can use it to (for example) clone all of the projects for which we are a member. Here's a shell script that does
that:

```sh
#!/bin/sh
jq < ./gitlab-projects.json -r \
    '.[] | select(.namespace.name == "my-namespace") |
        [.path_with_namespace, .ssh_url_to_repo] | @tsv' | \
while read -r path url ; do
    echo "$url -> $path"
    mkdir -p "$path"
    git clone "$url" "$path"
    sleep 2s        # go easy on the server
done
```
