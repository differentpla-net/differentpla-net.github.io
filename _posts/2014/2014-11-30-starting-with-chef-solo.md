---
title: Starting with chef-solo
date: 2014-11-30 18:39:07
tags: chef
published: false
---

*This is part of my [quest to sort out my website]({% post_url 2014/2014-11-13-website-rebuild %}).
Starting with chef-solo.*

Earlier in this quest, I talked about
[using Chef Solo with Vagrant]({% post_url 2014/2014-11-13-chef-solo-vagrant %}).

Now we need to use it in AWS. This is essentially the same as using it on any
other server, but it's different from using it in Vagrant, where the Vagrant
provision did some of the work for us.

**Note:** I _could_ have used vagrant to configure the AWS instance; I didn't
want to mess around with AWS Access Keys and secrets today. In future, I might
explore using vagrant to manage this.

## Revision Control

Because I'm that kind of person, and because it's the right thing to do, I'm
going to keep as much of this under revision control as possible. In short,
that means [github](https://github.com/rlipscombe/chef-repo).

    git clone https://github.com/rlipscombe/chef-repo

## solo.rb

`chef-solo` needs a configuration file, called `solo.rb`. It looks like this:

    ssl_verify_mode :verify_peer
    cookbook_path File.expand_path("../cookbooks", __FILE__)
    json_attribs File.expand_path("../node.json", __FILE__)

## node.json

We want to put our node into at least one role, so put the following in the
node.json file:

    {
        "run_list": [ "role[base]" ]
    }

## Roles

Create a file, `roles/base.rb` containing:

    name 'base'

    run_list(
            'apt'
    )

## Cookbooks

We're managing the cookbooks with berkshelf, so we'll need a `Berksfile` that looks like this:

    source "https://supermarket.getchef.com"

    cookbook 'apt'

And then we'll need to install those cookbooks:

    berks install
    berks vendor cookbooks/

## Running chef-solo

    cd ~/chef-repo
    sudo chef-solo -c solo.rb

## Hurrah!

And that should happily "converge", meaning that it's run `apt-get update` for us.
