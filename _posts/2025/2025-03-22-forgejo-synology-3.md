---
title: "Forgejo on Synology NAS, part 3: Backup and Restore"
short_title: "Backup and Restore"
date: 2025-03-22T16:42:47Z
tags: forgejo synology-nas
layout: series
series: forgejo-synology-nas
---

In [part 1]({% post_url 2025/2025-03-09-forgejo-synology-1 %}), and [part 2]({% post_url
2025/2025-03-09-forgejo-synology-2 %}), I'd got Forgejo running on my Synology NAS, with HTTPS and SSH access. It's not
being backed up (or restored); let's fix that.

The major problem with backing up Forgejo is that it uses both a database (PostgreSQL) and disk storage (for the git
repositories). This means that they need to be backed up together.

Given that I don't need 24/7 access to the Forgejo server, it seems to me that the most robust option is simply to stop
the containers, back everything up, and then start the containers again.

Recall, importantly, that the containers are using volume mounts for their storage, so the actual _containers_ don't
need backing up, just their data.

## Task Scheduler

1. In the DSM website, in _Control Panel_, in the _Services_ section, you'll find _Task Scheduler_.
2. Click _Create_ / _Scheduled Task_ / _User-defined script_.
3. On the General tab: name the task "Stop Forgejo"; set it to run as "root".
4. On the Schedule tab: set it to run daily, ad (e.g.) 04:00. This is a time when no-one is expected to be using the
   Forgejo instance.
5. On the Task Settings tab: enter `/volume1/docker/forgejo/scripts/stop.sh` as the user-defined script.

Repeat the above steps, but for a task named "Start Forgejo". Set it to run an hour later at (e.g.) 05:00. Use `/volume1/docker/forgejo/scripts/start.sh` as the script.

## Scripts

Create the following scripts in the `/volume1/docker/forgejo/scripts` directory and mark them as executable:

### `stop.sh`

```sh
#!/bin/bash

stop_container() {
        container=$1
        synowebapi --exec api=SYNO.Docker.Container version=1 method=stop name=$container
}

stop_container forgejo-forgejo-1
stop_container forgejo-postgres-1
```

Note that the script uses the Synology API to stop the container, and that it stops the Forgejo server before stopping
the PostgreSQL container.

Note also that I didn't bother stopping the HAProxy container. It's stateless, so there's no point. I also preferred the
"503 Service Unavailable" error that it reports, rather than the browser just spinning.

### `start.sh`

```sh
#!/bin/bash

start_container() {
        container=$1
        synowebapi --exec api=SYNO.Docker.Container version=1 method=start name=$container
}

start_container forgejo-postgres-1
start_container forgejo-forgejo-1
```

As above, this script uses the Synology API. It starts the PostgreSQL container before the Forgejo container (i.e. start
and stop are reversed).

## Snapshots

The scheduled tasks and scripts above mean that between 04:00 and 05:00, Forgejo is not running, and can be backed up
safely without worrying about consistency.

In addition to using Synology's Hyper Backup application to make an offline/offsite backup, I'm going to use the
_Snapshot Replication_ application to, well, make a snapshot of the containers.

1. On the _Snapshots_ page, in the _Shared Folders_ tab, select the "docker" entry.
2. Click the _Settings_ button.
3. Enable a daily snapshot at (e.g.) 04:15; this is shortly after the containers are stopped.
4. Configure protection and retention however you want. I chose 7 day protection and 45 day retention. I also selected
   the "Use GMT time to name the snapshots" option.

This adds an entry in _Task Scheduler_; we'll see it below.

### Testing snapshots

1. In _Task Scheduler_, run the "Stop Forgejo" task. Wait until Forgejo is stopped.
2. In _Task Scheduler_, run the "Share [docker] Snapshot" task.
3. In _Snapshot Replication_, on the _Recovery_ page, select "docker" and click the _Recover_ button.
4. From the list of snapshots, select the most-recent one. From the _Action_ drop-down, choose _Restore to this snapshot_.
5. In _Task Scheduler_, run the "Start Forgejo" task.

## Backups

To back up the containers, I'll use Synology's _Hyper Backup_ application. I'm looking for alternatives to S3 at the
moment, so I'll use an external hard disk for this exercise.

I (eventually) dug out a suitable 1TB external hard disk and plugged it into the NAS and formatted it as EXT4.

1. In _Hyper Backup_, click the _+_ button to add a backup task.
2. On the _Backup Type_ page, select "Folders and Packages".
3. On the _Backup Destination_ page, select "Local Shared Folder or USB".
4. On the _Backup Version Type_ page, select "Multiple versions".
5. On the _Backup Destination Settings_ page, specify "usbshare1" as the shared folder, and "diskstation_forgejo" as the
   directory.
6. On the _Data Backup_ page, select the `docker/forgejo` folder.
7. On the _Application Backup_ page, don't select anything.
8. On the _Backup Settings_ page, name the task "Forgejo". Set it to run at 04:30 (i.e. after the snapshot job above).
9. On the _Rotation Settings_ page, I chose _Smart Recycle_, which keeps daily backups for a month, then weekly if older
   than a month. I told it to keep 180 versions, which is just shy of 3 years' worth.

If you want to back up more than just the "docker" folder, you can do that here, too. For an external hard disk, that
makes sense; if you're backing up to the cloud, you might want separate backup jobs.

This adds an entry in _Task Scheduler_; we'll see it below.

### Testing backups

1. As above: stop Forgejo, and run the backup task.
2. In _Hyper Backup_, click on the _Restore_ button.
3. Select _Folder and Packages_.
4. Select the "Forgejo" task.
5. Do not restore system configuration.
6. Restore to the `docker/forgejo` directory.
7. Next, next, wait.
8. In _Task Scheduler_, run the "Start Forgejo" task.

## What's missing?

- Certificates from Let's Encrypt.
- Monitoring.
