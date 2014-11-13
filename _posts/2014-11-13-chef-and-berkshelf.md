---
title: Installing Chef and Berkshelf
date: 2014-11-13 10:38:12
---

*This is part of my [quest to sort out my website]({% post_url 2014-11-13-website-rebuild %}).
Installing Chef and Berkshelf.*

In order to avoid "server rot" -- which appears to be a term I just coined --
I'm going to avoid (as much as possible) manually building my web server. This
means that I should use something such as chef or puppet to manage it instead.

I'm going to use chef, entirely because that's what we use at Electric Imp,
meaning that if I have any questions, I can ask our devops team.

To install chef, you could simply:

    # DON'T DO THIS!
    sudo gem install chef

To manage chef's "cookbooks", I'm going to install
[**berkshelf**](http://berkshelf.com/):

    # DON'T DO THIS!
    sudo gem install berkshelf

But `sudo gem install` is not very source-control-friendly. Instead, use
[**bundler**](http://bundler.io/), which we should install like this:

    # DO DO THIS
    sudo gem install bundler

Then, create a `Gemfile` containing the following:

    source 'http://rubygems.org'

    gem 'chef'
    gem 'berkshelf'

And then run the following command:

    sudo bundle install

Note that this installs the gems system-wide. If you want to manage different
versions, then you should look at [**rvm**](http://rvm.io/).
