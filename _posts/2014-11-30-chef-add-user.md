---
title: Using chef to add a user
date: 2014-11-30 19:22:37
---

*This is part of my [quest to sort out my website]({% post_url 2014-11-13-website-rebuild %}).
Using chef to add a user.*

While I _could_ continue to log into my new EC2 instance as the default
`ubuntu` user, I'd prefer to use my own user account. Let's get chef to create
my user account.

## Using the 'users' community cookbook

**Note:** This is cribbed from [Managing Users and SSH Keys in a Hybrid
World](https://www.getchef.com/blog/2014/07/10/managing-users-and-ssh-keys-in-a-hybrid-world/)

Add the cookbook (and its dependencies) to `Berksfile`:

    cookbook 'users'
    cookbook 'sudo'
    cookbook 'chef-solo-search'

## Create a databag

    mkdir -p data_bags/users

## Configure the new user

Create a file `data_bags/users/roger.json`:

    {
        "id"       : "roger",
        "comment"  : "Roger Lipscombe",
        "shell"    : "/bin/bash",
        "groups"   : "sysadmin",
        "ssh_keys" : [
            "ssh-rsa AAAAB3...== roger"
        ]
    }

## Configure the role

In `roles/base.rb`, edit it as follows:

    run_list(
        # ...
        'chef-solo-search',
        'users::sysadmins'
    )

## sudoers

* TODO

## bash_profile

* TODO
