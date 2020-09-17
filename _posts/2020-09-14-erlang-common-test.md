---
title: "Using Erlang's Common Test for System Testing"
date: 2020-09-14T18:21:52Z
---

At Electric Imp (now part of Twilio), my team uses Erlang's Common Test for driving our system tests. These are (almost-)end-to-end tests that exercise (almost) the whole platform.

We do physical Internet-of-Things devices, so the "almost" allows us to exclude two things:

1. The physical devices. We use virtual devices instead.
2. The web UI. While we _have_ tests that exercise the UI, those belong to a different them; my team's tests talk to the underlying API.

Effectively what we do is stand up a cut-down clone of the production system, with data stores (Postgres, Redis, etc.), message brokers (RabbitMQ, VerneMQ, Redis), the customer-facing API (and its associated microservices), and a small number of instances of the device-facing services. All told, there are about 20 processes in the cut-down test environment.

I should note that we use a mixture of languages (Erlang, Elixir, Go, etc.), so this technique isn't restricted to only testing Erlang or Elixir processes.

At the moment, we run these processes directly on the developer's PC (or on the Jenkins worker), but we're in the process of converting them to run as docker containers, and this will also allow us to run the same tests against the staging environment, with some configuration changes.

Then we run a few hundred test cases against this setup.

## Running the test suites

Running the tests, grossly over-simplified, looks like this:

```sh
ct_run -dir $SUITES_DIR
```

Each test suite then makes extensive use of a domain-specific language (DSL), implemented as Erlang functions in order to set up the test fixture, simulate user (or device) actions and to then assert the results.

Here's a contrived example:

```erlang
Account = account_fixtures:create_account(Config),
Imp = device_fixtures:create_new_device(Account),
imp:connect_default(Imp, Config),
% ...
assert:eventually(imp:receives_code(Imp, ExpectedCode)),
```

...and so on.

These functions (in the `fixtures` application) are made available to the test suites by passing them to `ct_run`:

```sh
ct_run -pa fixtures/_build/default/lib/*/ebin \
       -include fixtures/include \
       ...
```

## Aside: `assert:eventually`

Because we're simulating multiple devices, and we're testing a distributed system, we're always going to run into race conditions if we try to assert things immediately, so this isn't going to work:

```erlang
?assert(server_assert:device_is_connected(Imp)).
```

(You can use _eunit_ assertions in Common Test...)

It's not going to work (reliably) because the simulated device _might not actually be connected_ at the point we run the assertion.

There are a number of ways around this, and we use two of them in various places in our tests:

1. You can poll something. This is what `assert:eventually` does; it periodically runs a "probe" that returns true or false. If it returns true, we're good. If it returns false, we try again after a short delay.
2. You can have the real server (or the virtual device) emit events when interesting things happen, and the tests can subscribe to those events, and block until they occur.

Or you could just inject a "sleep" into the test. We probably do this once or twice, but let's pretend we don't and never speak of this again.

## Mock Servers

One thing that makes the Electric Imp offering different is that each device is paired 1:1 with an "agent", which runs in the cloud, and allows disconnected operation.

Agents can make HTTP requests.

To test that, we need a mock HTTP server that can record the requests that it sees, and assertions that can inspect those requests.

This is done by having the fixtures application start up a mock HTTP server (which uses cowboy).

Unfortunately (as far as I can tell) `ct_run` doesn't support the `-s` switch from `erl`, so we use a simple CT hook for this:

```sh
ct_run ... \
    -ct_hooks \
        ei_ct_start_cth fixtures and \
        mock_http_cth
```

We have a generic CT hook that just calls `application:ensure_all_started` with the arguments it's given. Then there's a more specific CT hook (for the mock HTTP server) that clears the recorded history when each test case starts.

Because this runs under `ct_run`, it's directly accessible to the test suites.

## Running the daemons

One of the other CT hooks is responsible for actually _starting_ all of the processes under test. It uses erlexec to start (and stop) a script:

```sh
ct_run ... \
    -ct_hooks \
        ... and \
        ei_ct_exec_cth scripts/run-daemons
```

This is important if you're running on a local PC, because you want all of the processes killed if the tests fail.

If we're running against the docker-ised environment, or we want to leave the daemons running in a separate terminal window, the Makefile takes `RUN_DAEMONS=false` which causes it to omit this bit.

## Making the output pretty

If you've been using _eunit_ for any time, you've probably come across [eunit_formatters](https://github.com/seancribbs/eunit_formatters) or [unite](https://github.com/eproxus/unite), which make the output prettier.

We've done something similar for Common Test, with yet another CT hook. This one's called `ei_ct_report_cth`, and it hooks the suite and case start and stop events in order to output a nicely coloured summary, with Unicode ticks and crosses, and time taken.

We also have a CT event hook which uses terminal escape codes to update the title bar, in order to report the number of tests completed and tests yet to run, so that you can see it in your task bar.
