---
title: "synowebapi: Stopping Container Manager projects"
date: 2025-07-12T15:38
tags: synology-nas
---

Here's how to use `synowebapi` for stopping (and starting) projects in Container Manager on a Synology NAS.

## Background

To back up Forgejo and Immich on my Synology NAS, I've got a scheduled task that runs at 3am that uses `docker-compose
stop` to ensure that nothing's running before backing up the databases. This has the unfortunate side effect that I get
a bunch of "Container forgejo-postgres-1 stopped unexpectedly." warnings, one for each container.

## synowebapi

The Synology Diskstation software provides a `synowebapi` command line tool that you can run as root on the NAS itself
which invokes various APIs "correctly", so I figured I'd see if I could use that instead.

To figure out which API calls the normal web console uses, I opened Firefox Developer Tools and then navigated to the
"forgejo" project in Container Manager. It was already stopped, so I clicked the "Start" button. In the Network tab,
this showed a request to `SYNO.Docker.Project` with the following content:

```
{
	"api": "SYNO.Docker.Project",
	"method": "start_stream",
	"version": "1",
	"id": "\"bddfea05-8010-4dd9-a1c8-8d93867040b8\""
}
```

This is promising, except that it uses an ID rather than a name. On a hunch, I decided to try the following command:

```
synowebapi --exec api=SYNO.Docker.Project version=1 method=list | jq
```

This gave me the following output (various bits ellided):

```json
{
  "data": {
    //...
    "bddfea05-8010-4dd9-a1c8-8d93867040b8": {
      "containerIds": [
      ],
      "id": "bddfea05-8010-4dd9-a1c8-8d93867040b8",
      "name": "forgejo",
      //...
    },
    //...
  }
}
```

Bingo.

So we can find the project ID with the following:

```
# synowebapi --exec api=SYNO.Docker.Project version=1 method=list 2>/dev/null | jq -r '.data[] | select(.name == "forgejo") | .id'
bddfea05-8010-4dd9-a1c8-8d93867040b8
```

## Rewriting the scripts

This means that my `start.sh` script can look like this:

```sh
#!/bin/bash

forgejo_project_id=bddfea05-8010-4dd9-a1c8-8d93867040b8
immich_project_id=8ec29f37-76bb-49c9-bf07-9b30403fed54

synowebapi --exec api=SYNO.Docker.Project version=1 method=start "id=\"$forgejo_project_id\""
synowebapi --exec api=SYNO.Docker.Project version=1 method=start "id=\"$immich_project_id\""
```

Note the awkward quoting (which you can actually see in the original captured request). Without this, you get a "Not a
json value" warning from `synowebapi`. Worse than that, it sometimes fails -- I suspect that it thinks that
`8ec29f37...` is a number in scientific notation, rather than a string. Therefore: quotes.

Yes, I could have put the `jq` query in the script, but the IDs aren't going to change, so there's little point.

The `stop.sh` script is almost identical, except that it has `method=stop`.
