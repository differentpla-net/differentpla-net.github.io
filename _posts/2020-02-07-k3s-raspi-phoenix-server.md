---
title: "Using k3s on Raspberry Pi: Phoenix Server"
date: 2020-02-07T11:17:00
layout: series
series: k3s
tags: k3s raspberry-pi docker elixir phoenix
---

Note: This is basically the same as [the node.js server]({% post_url 2020-02-06-k3s-raspi-node-server %}).

## Creating the Phoenix app

It's entirely possible to create the app inside the docker container, or on a completely different machine, but I'm going to do it locally, which means [installing Erlang, Elixir and Phoenix first]({% post_url 2020-02-07-rpi-elixir %}).

Then:

```
mix phx.new phoenix_server --no-ecto
```

If you answer 'Y' to the "Fetch and install dependencies?" prompt, this'll take a while, but it'll save doing it later in the docker build step.

## `Dockerfile`

```
# Use an official Elixir runtime as a parent image
FROM elixir:latest

# Install hex, rebar and phoenix.
RUN mix local.hex --force \
    && mix local.rebar --force \
    && mix archive.install --force hex phx_new 1.4.12

# Install nvm
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.2/install.sh | bash

# Install node
ENV NODE_VERSION=v12.14.1
RUN set -e \
    && NVM_DIR="$HOME/.nvm" \
    && . "$NVM_DIR/nvm.sh" \
    && nvm install $NODE_VERSION

# Create app directory and copy the Elixir projects into it
RUN mkdir /app
COPY . /app
WORKDIR /app

# Compile the project
RUN mix do deps.get, compile

RUN set -e \
    && NVM_DIR="$HOME/.nvm" \
    && . "$NVM_DIR/nvm.sh" \
    && cd assets \
    && npm install \
    && node node_modules/webpack/bin/webpack.js --mode development

EXPOSE 4000

CMD ["/app/docker-entrypoint.sh"]
```

## `docker-entrypoint.sh`

```
#!/bin/bash

set -e

export NVM_DIR="$HOME/.nvm"
. "$NVM_DIR/nvm.sh"

cd /app
exec mix phx.server
```

Make it executable:

```
chmod +x docker-entrypoint.sh
```

Note that the above runs the Phoenix app in development mode.

## Create the docker image

Note that we switch to using a hyphen in the name at this point.

```
docker build -t phoenix-server .
```

## Push it to the private docker repository

```
docker tag phoenix-server rpi201:5000/phoenix-server
docker push rpi201:5000/phoenix-server
```

## Create a deployment

```
sudo kubectl create deployment phoenix-server --image=rpi201:5000/phoenix-server
```

## Expose the deployment

```
sudo kubectl expose deployment phoenix-server --port 4000
```

## Check it's working

```
sudo kubectl get endpoints phoenix-server
curl 10.42.3.15:4000
```

## Links

- <https://pspdfkit.com/blog/2018/how-to-run-your-phoenix-application-with-docker/>
