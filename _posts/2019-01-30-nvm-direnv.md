use_nodejs() {
    NODE_VERSION="$1"

    type nvm >/dev/null 2>&1 || . ~/.nvm/nvm.sh
    nvm use "$NODE_VERSION"
}
