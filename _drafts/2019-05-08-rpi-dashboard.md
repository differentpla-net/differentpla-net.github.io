---
title: "Raspberry Pi Dashboard"
date: 2019-05-08T14:03:00.000Z
layout: series
series: rpi-dashboard
---

This is just notes at the moment; it needs fleshing out

## Full Screen / Kiosk Mode

    mkdir -p /home/pi/.config/lxsession/LXDE-pi/

    nano /home/pi/.config/lxsession/LXDE-pi/autostart

    @xset s odd
    @xset -dpms
    @xset s noblank
    @chromium-browser --incognito --kiosk http://localhost:4039

You can use `--start-fullscreen` instead of `--kiosk` if you want to be able to get _out_ of fullscreen mode by pressing F11.

## Jenkins API

To use curl with Jenkins, you'll need an API key:

1. Browse to Jenkins and log in.
2. Click on your username (top right).
3. Click on Configure.
4. In the "API Token" section, click "Add new Token", give it a name and
   then click "Generate". Copy the token somewhere safe.

## Using it with curl

    curl -s -u "$JENKINS_API_USER:$JENKINS_API_TOKEN" \
        "$JENKINS_URL/job/$JOB_NAME/lastCompletedBuild/api/json"

    curl -s -u "$JENKINS_API_USER:$JENKINS_API_TOKEN" \
        "$JENKINS_URL/view/$JENKINS_VIEW/api/json" \
        | jq '.jobs | .[] | {name, color}'
