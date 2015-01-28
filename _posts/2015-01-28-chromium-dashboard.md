---
title: Running Chromium as a Dashboard
date: 2015-01-28 08:26
---

On my desk I have a dashboard (an "information radiator") that displays various
useful things, such as the build status of various Jenkins jobs, clocks showing
important timezones (Electric Imp is a distributed company), and so on.

It's built from an Acer nettop, and it runs Chromium in kiosk mode. Here's how
I got it to start everything automatically.

## Dashboard Web Server

The dashboard is implemented using [dashing](http://dashing.io/), and I use an
upstart script (in `/etc/init/dashing.conf`) to ensure that it's running at
bootup:

    #!upstart
    description "Dashing dashboards"
    author "Roger Lipscombe"

    respawn
    start on runlevel [23]

    setuid dashboard
    setgid dashboard

    script
      cd /home/dashboard/dashboard/
      dashing start
    end script

Note that the script assumes that you have a `dashboard` user, and that dashing
is installed in `/home/dashboard/dashboard`.

## Starting X

Great! I've got a web server running the dashboard software; now I need to
display it somewhere. To do this I run an X session on the box.

This is done with two files: `/etc/init/startx.conf` starts X:

    #!upstart
    description "Start X without a display manager or a window manager"
    author "Roger Lipscombe"

    # start/stop lifted from Mint's mdm.conf:
    start on ((filesystem
               and runlevel [!06]
               and started dbus
               and (drm-device-added card0 PRIMARY_DEVICE_FOR_DISPLAY=1
                    or stopped udev-fallback-graphics))
              or runlevel PREVLEVEL=S)

    stop on runlevel [016]

    script
        USER="dashboard"
        exec /bin/su -s /bin/sh -l -c "/usr/bin/startx" $USER
    end script

## Running Chromium

Chromium is then run from `/home/dashboard/.xinitrc`:

    #!/bin/sh

    # Turn off screen blanking and power saving
    xset s off
    xset -dpms

    # Rotate the screen (optional)
    xrandr --output HDMI-1 --rotate left

    # Hide the mouse cursor.
    unclutter -grab &

    # Start the web browser, with an address...
    while true; do
        # Sleep for a bit; this gives the network a chance to recover, and
        # also allows us to do stuff remotely (like clear the cache) before
        # Chrome restarts.
        sleep 5

        # See http://stackoverflow.com/q/9792667/8446
        sed -i 's/"exited_cleanly": false/"exited_cleanly": true/' \
            ~/.config/chromium/Default/Preferences

        # This doesn't go into the background, which is good.
        chromium-browser --kiosk http://localhost:3030/
    done

