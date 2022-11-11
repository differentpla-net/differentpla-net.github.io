## Aside: secrets

Mounted at /secrets, we can take a look:

kubectl --namespace docker-registry exec --stdin --tty docker-registry-5f6db8fc98-svgrz -- /bin/sh

Because it's a TLS-type secret, it gets exposed as two files, which is good. Are other secret types multi-part?
