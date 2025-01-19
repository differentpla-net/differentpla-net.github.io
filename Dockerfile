FROM docker.io/ruby:3.3.7-alpine3.21

RUN apk update \
    && apk --no-cache --update add build-base git

RUN gem update --system 3.6.3
RUN gem install bundler:2.6.3

WORKDIR /web
COPY Gemfile Gemfile
COPY Gemfile.lock Gemfile.lock

RUN bundle install
