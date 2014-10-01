# Installing Graphite on Mac OS X

## Install the packages

    sudo easy_install carbon
    sudo easy_install whisper
    sudo easy_install graphite-web

## Configure Apache

On Mac OS X, Apache is installed by default, but is fairly barebones. It'll do
for our purposes.

### Set up the vhost

The graphite-web package installs an example `example-graphite-vhost.conf` file
in your Python site-packages directory.

On Mac OS X, the file ended up in:

    /Library/Python/2.7/site-packages/
        graphite_web-0.9.12-py2.7.egg/examples/example-graphite-vhost.conf

(wrapped)

### Enable virtual hosts

In the `/etc/apache2/httpd.config` file, find the line `# Virtual hosts`. It's
quite close to the bottom. Add an `Include` directive, so that it looks like
the following:

    # Virtual hosts
    #Include /private/etc/apache2/extra/httpd-vhosts.conf
    Include /private/etc/apache2/extra/vhosts.d/*.conf

Then:

    sudo mkdir -p /private/etc/apache2/extra/vhosts.d/
    sudo cp /Library/Python/2.7/site-packages/graphite_web-0.9.12-py2.7.egg/examples/example-graphite-vhost.conf \
        /private/etc/apache2/extra/vhosts.d/

### Start Apache

    sudo apachectl start

