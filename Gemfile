source 'https://rubygems.org'
gem 'github-pages', group: :jekyll_plugins

# This will cause a "github-pages can't satisfy your Gemfile's dependencies." warning during Github Actions.
# What I don't understand is that the octicons work _anyway_.
# See https://github.com/github/pages-gem/pull/483, which shows that it works, but I still don't know _how_.
gem 'jekyll-octicons', group: :jekyll_plugins

# Ruby 3.x no longer includes these by default, so add them explicitly
gem 'csv'
gem 'webrick'

# These will cause the same error, but since they're not used for the actual blog, you can ignore it.
gem 'rake'
gem 'html-proofer'
