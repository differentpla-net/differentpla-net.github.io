

Note that `$TARGETPLATFORM` isn't available inside the rest of the Dockerfile unless you specify ARG.

Does that mean that environment vars, in general, are available in FROM, even if not marked with ARG?

But why, with manifests, did I need to add `FROM --platform`? Seem to be getting away without it?
