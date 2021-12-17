---
title: Using Chef Solo with Vagrant
date: 2014-11-13 11:17:13
tags: chef
---

*This is part of my [quest to sort out my website]({% post_url 2014-11-13-website-rebuild %}).
Using Chef Solo with Vagrant.*

Having decided to use Chef Solo to manage my web server, at least initially,
it's time to integrate it with vagrant.

There is a `vagrant-berkshelf` plugin, but I found it unreliable, so I opted
not to use it.

## Configuration

To use the chef-solo provisioner, edit the `Vagrantfile`, adding the following
section:

    # Chef Solo provisioner
    config.vm.provision :chef_solo do |chef|
      # We use 'berks vendor chef/cookbooks', so look there for cookbooks:
      chef.cookbooks_path = "chef/cookbooks"
      # For symmetry, put the roles in the same place.
      chef.roles_path = "chef/roles"
    end

Note that I've opted to keep all of the chef-related files in a `chef` folder,
so the defaults won't work.

You need to ensure that the directories exist:

    mkdir -p chef/cookbooks
    mkdir -p chef/roles

And you'll need to restart the VM in order to mount these directories inside
the VM:

    vagrant reload

## Our first cookbook and role

If we attempt to provision the VM at this point, chef won't work. It needs at
least one cookbook, so let's do that.

First, we'll create a new `base` role for this VM. Create a file
`chef/roles/base.rb` containing the following:

    name 'base'

    run_list(
        'apt'
    )

When the machine starts, this will automatically run `apt-get update` for us,
if it hasn't been run recently.

Next, we need to add the machine to the role. To do this, edit `Vagrantfile`,
and in the `:chef_solo` section, add the following:

      chef.add_role "base"

Then we need to make the cookbook available. Create a `Berksfile` containing
the following:

    source "https://supermarket.getchef.com"

    cookbook 'apt'

## Installing the cookbooks

Run the following commands:

    berks install
    berks vendor chef/cookbooks

This will download the cookbooks listed in `Berksfile` (and their
dependencies). They'll be installed somewhere in `~/.berkshelf`. The second
command copies them to the `chef/cookbooks` directory, where the chef-solo
provisioner running in the VM can find them.

## Run the provisioner

Then you can run the provisioner:

    vagrant provision

And that's pretty much it for now.
