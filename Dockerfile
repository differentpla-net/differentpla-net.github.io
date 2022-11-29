FROM docker.io/ruby:2.7.4-alpine3.14 AS gems

RUN apk update \
    && apk --no-cache --update add build-base git

RUN gem install bundler:2.2.32

WORKDIR /web
COPY Gemfile Gemfile
COPY Gemfile.lock Gemfile.lock

RUN bundle install
