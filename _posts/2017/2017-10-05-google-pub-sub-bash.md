---
title: Using Google Pub/Sub from bash
date: 2017-10-05 14:35+0000
tags: jwt bash google-pub-sub
---

## Background

Several of our customers want their [Electric Imp](https://electricimp.com/) agents to be able to talk to Google's APIs, in particular Google Pub/Sub.

As part of that, I'm implementing `RS256` signing for Electric Imp agents, so that agents can generate and sign JSON Web Tokens (JWT).

<div class="callout callout-warning" markdown="span">
Disclaimer: any discussion of Electric Imp features, roadmap, etc. is basically rumour and hearsay, is totally unofficial and unsanctioned, and shouldn't be relied on.
</div>

<div class="callout callout-info" markdown="span">
Except in this case. We shipped it. See the documentation for [`crypto.sign()`](https://developer.electricimp.com/api/crypto/sign).
</div>

Rather than just dive in and knock out some C++ (and Squirrel) code, I
thought I'd explore how it all works first.

Using bash, because that's how I roll.

## Google Pub/Sub Topics and Subscriptions

To use Pub/Sub, you'll need an overall project, at least one pub/sub topic, and at least one subscription.

Go to https://console.cloud.google.com/ and either create a new project or
select an existing project.

Google will generate a unique ID for your project. For example, it might call it "bamboo-analyst-182014".

From the "hamburger" menu, choose the "Pub/Sub" entry, and then choose
"Topics". Then click on "Create Topic" and enter a suitable name. I called mine "friday" (because it was Friday at the time).

Then click on your topic and click the "Create Subscription" button. Enter a
suitable name for your subscription.

Creating your subscriptions via the console isn't particularly scalable. In a real application, you'd probably do this in code.

## Service Account

To access Pub/Sub, you'll need to create a service account.

Go to https://console.developers.google.com/iam-admin/serviceaccounts/ and
click "Create Service Account". Enter a suitable name and select the
appropriate roles. At this point, I wasn't sure which ones I wanted, so I
granted all of the Pub/Sub roles. You can fix that later in the "IAM" tab.

You'll need a public/private keypair for your service account. You can create this when you initially create the service account, or you can create a new keypair later. A service account can have multiple keys, if necessary.

The console will download the private key (as JSON, by default) to your PC. You need to keep this key somewhere safe, because it's (a) the only copy; (b) intended to be **private**.

## Creating a JSON Web Token (JWT) in bash

To access the Google Pub/Sub API, we need an access token. To get an access
token, we need to present a JWT token.

### Install `jq`

Install `jq`; see https://stedolan.github.io/jq/

### `base64url` helper function

You'll need this:

```bash
base64url() {
    base64 -w 0 | tr '+/' '-_' | tr -d '='
}
```

### Service Account Email / Private Key

You'll need to deal with the JSON file you just downloaded:

```bash
PRIVATE_KEY_JSON_PATH=$HOME/Downloads/foo-bar-bbec76ee9047.json
service_account_email=$(jq -r '.client_email' < $PRIVATE_KEY_JSON_PATH)
jq -r '.private_key' < $PRIVATE_KEY_JSON_PATH > my.key
```

### JWT Header

You need a header:

```bash
jwt_header=$(echo -n '{"alg":"RS256","typ":"JWT"}' | base64url)
```

Don't forget the `-n`.

Obviously, the input is a constant, so the output will always be the following:

```
eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9
```

### JWT Claims

```bash
# The 'jq -Mc' uses jq to validate the JSON, and removes the whitespace (and colour).
jwt_claims=$(cat <<EOF |
{
  "iss":"$service_account_email",
  "scope":"https://www.googleapis.com/auth/pubsub",
  "aud":"https://www.googleapis.com/oauth2/v4/token",
  "exp":$(date +%s --date="+600 seconds"),
  "iat":$(date +%s)
}
EOF
jq -Mc '.' | base64url)
```

### JWT Signature

```bash
jwt_signature=$(echo -n "${jwt_header}.${jwt_claims}" | \
    openssl dgst -sha256 -sign my.key | base64url)
```

### JWT

```bash
jwt="${jwt_header}.${jwt_claims}.${jwt_signature}"
```

### Exchange the JWT for an access token

```bash
token_json=$(curl -s -X POST \
    https://www.googleapis.com/oauth2/v4/token \
    --data-urlencode \
        "grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer" \
    --data-urlencode \
        "assertion=$jwt")
access_token=$(echo $token_json | jq -r '.access_token')
```

### Subscribe

```bash
# set these appropriately
project=<project-name>
sub=<subscription-name>

subscription=projects/$project/subscriptions/$sub
curl -s -X POST \
    https://pubsub.googleapis.com/v1/$subscription:pull \
    -H "Authorization: Bearer ${access_token}" \
    -H "Content-Type: application/json" \
    -d '{"maxMessages":10}'
```

Note that you must specify the content type, or you'll get a confusing error about "Invalid JSON payload received.". You also have to specify a value for `maxMessages`.

When you run the `curl` pull command, it will block, waiting for messages to be published.

### Publish

You can publish a message from the Pub/Sub page in the Google Cloud Console. Select the relevant topic and click the "Publish Message" button.

A message has a body and can have zero or more key/value pairs (both strings).

### Receiving a message

When you publish the message, your `curl` pull command should complete (if it hasn't already timed out); it will print out something like the following:

```json
{
  "receivedMessages": [
    {
      "ackId": "QV5A...LLD5-PT5F",
      "message": {
        "data": "SGVsbG8gV29ybGQh",
        "attributes": {
          "foo": "12"
        },
        "messageId": "151442302373991",
        "publishTime": "2017-10-05T15:37:04.514Z"
      }
    }
  ]
}
```

You can see the `attributes`, and you can see the `message`. It's base64-encoded, so you'll need to decode it:

```
$ base64 -d <<< "SGVsbG8gV29ybGQh"
Hello World!
```

### Acknowledging a message

If you run the `curl` pull command again, you'll get the same message again. This is because you didn't acknowledge the message.

To do this, take the `ackId` field from the message and run the following:

```bash
acknowledge='{"ackIds": ["QV5A...LLD5-PT5F"]}'
curl -s -X POST \
    https://pubsub.googleapis.com/v1/$subscription:acknowledge \
    -H "Authorization: Bearer ${access_token}" \
    -H "Content-Type: application/json" \
    -d $acknowledge
```

Note that you have to do this before the ack deadline expires. The deadline is per-subscription, and defaults to 10 seconds.

### Publishing a message

```bash
# set these appropriately
project=<project-name>
top=<subscription-name>

topic=projects/$project/topics/$top

cat <<EOF |
{
  "messages": [
    {
      "data": "$(echo -n "Hello World!" | base64)"
    }
  ]
}
EOF
jq -Mc '.' |
curl -s -X POST \
    https://pubsub.googleapis.com/v1/$topic:publish \
    -H "Authorization: Bearer ${access_token}" \
    -H "Content-Type: application/json" \
    -d @-
```

## Message Ordering

You might notice that, when you pull messages from your subscription (and particularly if you're not acknowledging them), you don't always get the complete list of messages, and they're not always in a consistent order.

You might then ask "are messages guaranteed to be delivered in order?". Google answers that question here: [Message Ordering](https://cloud.google.com/pubsub/docs/ordering). (**tl;dr: no**).

## References

- [Google Cloud Authentication Overview](https://cloud.google.com/docs/authentication/)
- [Google Cloud Pub/Sub API](https://cloud.google.com/pubsub/docs/reference/rest/)
- [Using OAuth 2.0 for Server to Server Applications](https://developers.google.com/identity/protocols/OAuth2ServiceAccount), particularly "Preparing to make an authorized API call (HTTP/REST)".
