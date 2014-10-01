# Installing InfluxDB and Grafana on Ubuntu

## Installing InfluxDB

Follow the instructions at http://influxdb.com/download/ to install InfluxDB.
For me, this was as simple as:

    wget http://s3.amazonaws.com/influxdb/influxdb_latest_amd64.deb
    sudo dpkg -i influxdb_latest_amd64.deb

This installs InfluxDB into /opt/influxdb. To start it:

    sudo /etc/init.d/influxdb start

Check it's working by going to the admin console at http://localhost:8083/. Log
in as user 'root', password 'root'.

Obviously in production, you'll want to change the password to something more
secure, set up HTTPS, etc. This is left as an exercise for the reader.

## Configure the graphite plugin

Some of our applications talk to **statsd**, which then talks to **graphite**;
some of our applications talk directly to **graphite**. Either way, this needs
to be a drop-in replacement for **graphite** for those applications.

To support this, we can enable the graphite plugin in InfluxDB. Edit the
`/opt/influxdb/shared/config.toml` file. Note: **not** the version-specific
config file, as follows:

Change the `[input_plugins.graphite]` section to read as follows:

    # Configure the graphite api
    [input_plugins.graphite]
    enabled = true
    port = 2003
    database = "graphite"

You then need to create the "graphite" database. Do this from the admin
console.

Restart InfluxDB:

    sudo /etc/init.d/influxdb restart

If you look in `/opt/influxdb/shared/log.txt`, you should see "Starting
Graphite Listener on port 2003". If, instead, you see "Cannot start graphite
server. please check your configuration", then you should check your
configuration.

## Explore the data

Assuming that you've got some applications publishing data to
what-used-to-be-graphite, then that data should now appear in InfluxDB.

In the admin console, click on 'Databases', then click on 'Explorer Data >>'
for the 'graphite' database. This will take you to the **Data Interface** page.

In the 'Read Points' section, enter `list series;` as a query and press the
'Execute Query' button. You should see a list of the metrics that have been
reported.

Picking one of these arbitrarily, we can see that there is some time-series
data reported. For example, I can use the following query to check on the
memory usage of a local instance of `imp_server`:

    select * from folsomite.imp_server_roger-pc.vm.memory.total;

...which shows the data series reported for this metric, along with a pretty
graph.

## Installing Grafana

From http://grafana.org/download/, grab the latest release. Since we're on
Ubuntu, we'll want the TAR file:

    wget http://grafanarel.s3.amazonaws.com/grafana-1.6.1.tar.gz

Unpack that somewhere:

    cd /opt
    sudo tar xfz grafana-1.6.1.tar.gz

That needs to be configured to be served by a webserver. I'm using nginx, so
I'll simply set up a new virtual host.

### nginx configuration

This is `/etc/nginx/sites-available/grafana`:

    server {
            listen 80;
            server_name grafana;

            root /opt/grafana-1.6.1;
            index index.html index.htm;

            location / {
                    try_files $uri $uri/ =404;
            }
    }

Then:

    ln -s /etc/nginx/sites-available/grafana /etc/nginx/sites-enabled

I've also added `grafana` as an alias for `127.0.0.1` to `/etc/hosts`, because
I can't be bothered to mess with my DNS server.

## Configure grafana

    cd /opt/grafana-1.6.1
    cp config.sample.js config.js

Then edit the `datasources` section so that it looks like this:

    datasources: {
        influxdb: {
            type: 'influxdb',
            url: "http://localhost:8086/db/graphite",
            username: 'root',
            password: 'root'
        },
    },

## Is this thing on?

Go to http://grafana; you should see the default grafana dashboard.

1. Click on the "Graphite test" label at the top of the second row.
2. Click "Edit" on the menu that appears.
3. Enter a new query for (e.g.) `folsomite.imp_server_roger-pc.vm.memory.total`
4. You should see a graph appear.

## Installing elasticsearch

If you want to save dashboards, you'll need Elasticsearch installed as well.

**TODO**

