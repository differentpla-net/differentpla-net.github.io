---
title: "Release Process: Code Signing - Key Security"
date: 2010-05-26T10:10:41.000Z
redirect_from: /content/2010/05/release-process-code-signing-key-security
---
The key used for signing your releases should be kept secure. It should be impossible for a rogue developer or 3rd party to use this key unofficially.

You should be pragmatic about this: You _could_ keep the key in a secured location (e.g. on a computer that’s not connected to the network). Depending on your threat model, however, this might be overkill. It also adds overhead to your release process, including a manual step (and manual steps are often a bad thing).

On the other hand, you might decide that auditing is sufficient. If you’re using an automated build system, you can give the build agents access to the key. Since everything that’s built has to come through your source control system, you’ve got a ready made audit trail of what went into a particular release and was signed with that key.
