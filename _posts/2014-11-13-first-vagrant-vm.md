---
title: First Vagrant VM
date: 2014-11-13 10:18:45
---

*This is part of my [quest to sort out my website]({% post_url 2014-11-13-website-rebuild %}).
Bringing up the VM.*

To bring up a new VM using vagrant, we're going to run `vagrant init`. This
writes a skeleton `Vagrantfile` to the current directory, so we probably want
to invent a directory to keep it in:

    mkdir -p ~/Vagrant/trusty64
    cd ~/Vagrant/trusty64
    vagrant init ubuntu/trusty64

This will create an example Vagrantfile. Before continuing, we'll make a few
tweaks to it.

Note that if you don't want the commented-out example sections, use the
following instead:

    vagrant init --minimal ubuntu/trusty64

## Machine Name

We need to change the machine name in three places:

 - The [name that Vagrant uses](http://stackoverflow.com/a/20431791/8446).
 - The name that VirtualBox uses. The default is [the name of the containing
   folder plus a
   timestamp](https://docs.vagrantup.com/v2/virtualbox/configuration.html).
 - The name that the machine itself uses.

To do this, we'll need to make some changes to the `Vagrantfile`, as follows:

    HOST_NAME = "trusty64"

    Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
      # Ordinarily, Vagrant refers to this machine as 'default'. This changes that
      # name.
      config.vm.define HOST_NAME do |h|
      end

      # This is the hostname that the virtual machine uses internally.
      config.vm.hostname = HOST_NAME

      # VirtualBox configuration.
      config.vm.provider "virtualbox" do |v|
        # Use the specified name in VirtualBox as well.
        v.name = HOST_NAME
      end
    end

## Memory, CPU cores, etc.

Again, this is from Vagrant's [documentation for Virtual
Configuration](https://docs.vagrantup.com/v2/virtualbox/configuration.html):

    config.vm.provider "virtualbox" do |v|
      v.memory = 2048
      v.cpus = 2
    end

## Modeline tweaks

I prefer the modeline to be at the bottom of the file, so I usually move it.
And, because I'm using vim, I edit it slightly:

    # vim: sw=2:ts=2:sts=2:set ft=ruby:

I don't know what the equivalent emacs modeline would be ;-)

## Turning on the VM

    vagrant up

At this point you should have a running VM, configured appropriately. You can
connect to it with:

    vagrant ssh
