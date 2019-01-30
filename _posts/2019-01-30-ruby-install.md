
Use ruby-install:

    mkdir -p ~/Source/postmodern
    cd ~/Source/postmodern
    wget -O ruby-install-0.7.0.tar.gz https://github.com/postmodern/ruby-install/archive/v0.7.0.tar.gz
    tar -xzvf ruby-install-0.7.0.tar.gz
    cd ruby-install-0.7.0/

    ./bin/ruby-install --install-dir ~/.rbenv/versions/2.4.4 ruby 2.4.4

Then, later:

    gem install bundler
    bundle env | grep '^Ruby\s'
    bundle install
