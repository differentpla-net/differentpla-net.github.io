use_ruby() {
    RUBY_VERSION="$1"
    if [ -s "$HOME/.rbenv/versions/$RUBY_VERSION" ]; then
        load_prefix "$HOME/.rbenv/versions/$RUBY_VERSION/ruby-$RUBY_VERSION"
    else
        tput setaf 1
        echo "Ruby $RUBY_VERSION not available; using default"
        tput sgr0
    fi
}
