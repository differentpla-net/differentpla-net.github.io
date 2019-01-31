---
title: Installing Ruby with ruby-install
date: 2019-01-30 14:53
---

Install `ruby-install`:

    mkdir -p ~/.direnv/
    cd ~/.direnv/
    wget -O ruby-install-0.7.0.tar.gz \
        https://github.com/postmodern/ruby-install/archive/v0.7.0.tar.gz

    tar -xzvf ruby-install-0.7.0.tar.gz
    ln -s ruby-install-0.7.0 ruby-install

Create `~/.direnv/bin/ruby-install`:

```
cat > ~/.direnv/bin/ruby-install <<'EOF'
#!/bin/sh
~/.direnv/ruby-install/bin/ruby-install \
    --no-install-deps --src-dir ~/.rubies/src --cleanup $@
EOF

chmod +x ~/.direnv/bin/ruby-install
```

The `--no-install-deps` above prevents `ruby-install` from attempting to, well, install dependencies. If you leave this off, it'll run a `sudo apt install` (or whatever) on your behalf, which leaves you with an annoying password prompt.

The list of dependencies is in the `dependencies.txt` file, so we can install them ourselves:

    sudo apt install $(
    grep '^apt:' \
        ~/.direnv/ruby-install/share/ruby-install/ruby/dependencies.txt | \
        cut -d: -f2- )

Then install your chosen Ruby version:

    ~/.direnv/bin/ruby-install ruby 2.4.4

The above command will install Ruby 2.4.4 to `~/.rubies/ruby-2.4.4`, which is where `direnv` [will look for it]({% post_url 2019-01-30-ruby-direnv %}).
