



----

When I was building erlang-cluster, I had to specify --platform in the FROM, and also ARG TARGETPLATFORM.

Was that due to podman versions?

What does docker do? Bear in mind I've got containerd on one, and not on the other.

Given this:

I think the problem is that, absent manifests, if you specify multiple platforms you get one arbitrarily. And then that gets pulled to the wrong node type and it doesn't work.

If you build it with both platforms, it's arbitrary which one actually gets written to the image:


So you need manifests. Does docker deal with this differently?

Does a later podman deal with this differently?

On the Mac, it pulls the upstream arch arbitrarily.

I've just managed to build a fresh image, without specifying the platform in FROM, and it built arm64, presumably because that's the one I had last pulled. That is: if you don't specify --platform in podman build, it's arbitrary which one you get.

Yep.

So, if you do specify --platform, you get what you want, right?

(still podman)

If you specify a single platform on Windows, you get that one. If you specify both, you get one arbitrarily; see above.

Does that ^^ behaviour apply on later podman (i.e. Mac)?

TODO

----

Does specifying the platform in the Dockerfile fix it?

So the FROM --platform thing is probably required in that case, and the VS Code warning about it is bogus. Or, we need to be explicit about which platform we're building for.

But why, with manifests, did I need to add `FROM --platform`? Seem to be getting away without it?
