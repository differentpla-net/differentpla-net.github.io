---
title: Installing Chef in AWS
date: 2014-11-30 18:35:11
tags: chef
---

*This is part of my [quest to sort out my website]({% post_url 2014-11-13-website-rebuild %}).
Installing Chef in AWS.*

## Updating Ubuntu

First, we'll update Ubuntu:

    sudo apt-get update
    sudo apt-get upgrade

(You might want to use `apt-get dist-upgrade` if there's a kernel security patch waiting.)

Obviously, we can get chef to do this itself, but there's no point in
installing outdated chef prequisites (e.g. ruby) only for chef to have to
update them again in a moment.

## Installing git

We're going to need it later, so we might as well install it now:

    sudo apt-get install git

## Installing ChefDK

I want to use [berkshelf](http://berkshelf.com/) to manage my cookbooks, but
there's [a problem] installing it as a gem on smaller AWS boxes.

Looks like we'll have to install
[ChefDK](http://downloads.getchef.com/chef-dk/) instead.

At the time of writing, the latest version of ChefDK was 0.3.5:

    wget https://opscode-omnibus-packages.s3.amazonaws.com/ubuntu/12.04/x86_64/chefdk_0.3.5-1_amd64.deb
    sudo dpkg -i chefdk_0.3.5-1_amd64.deb
