---
title: Installing Vagrant on Ubuntu
date: 2014-11-13 10:15:31
---

*This is part of my [quest to sort out my website]({% post_url 2014-11-13-website-rebuild %}).
Installing Vagrant.*

## Installing Oracle VirtualBox

Before installing Vagrant, you'll probably want to install VirtualBox locally.
Get it from the [VirtualBox Linux Downloads
page](https://www.virtualbox.org/wiki/Linux_Downloads). Version 4.3.18 was the
latest version at the time of writing.

    wget http://download.virtualbox.org/virtualbox/4.3.18/virtualbox-4.3_4.3.18-96516~Ubuntu~raring_amd64.deb
    sudo dpkg -i virtualbox-4.3_4.3.18-96516~Ubuntu~raring_amd64.deb

*Note that if you're already using VirtualBox (so this is just an upgrade),
you'll need to shutdown any guest VMs and close the VirtualBox window before
this will succeed.*

## Installing Vagrant

The package repository for Ubuntu 14.04 (trusty) includes Vagrant 1.4.3, which is old.

The latest (at the time of writing) is 1.6.5, you should probably install it
direct from the [Vagrant downloads
page](https://www.vagrantup.com/downloads.html).

    wget https://dl.bintray.com/mitchellh/vagrant/vagrant_1.6.5_x86_64.deb
    sudo dpkg -i vagrant_1.6.5_x86_64.deb
