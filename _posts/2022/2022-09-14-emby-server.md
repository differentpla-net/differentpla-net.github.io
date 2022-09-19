---
title: "Installing Emby Server"
date: 2022-09-14T12:16:00Z
tags: emby
---

The Linux box running my [Emby](https://emby.media/) Server is ill, so I need to reinstall it. _This_ time, I'll make
some notes.

This will be of limited use to most people. It's mostly aimed at future-me, in case I need to do this again.

## Hardware

The new server will be an Intel NUC11TNKi3 with an 11th-gen Core i3. It's got 16GiB RAM, and 2TiB NVMe storage.

I've got an HDHomeRun Connect Quatro (4x DVB-T tuner) for watching and recording Live TV.

The client will be running on an nVidia Shield TV (2017).

## Operating System

Ubuntu 22.04.1 LTS Server. I opted against the "minimized" version.

## Storage

Note that by default (and by design), the root filesystem only occupies 100GiB, so I'll need to set up another LVM
volume to hold the data for Emby.

Quick reminder: LVM uses physical volumes (disk partitions, usually), which are grouped into volume groups. A PV belongs to a single VG. A VG can contain multiple PVs.

A volume group can provide space to one or more logical volumes.

That is: `PV >-- VG --< LV`. Basically, you're combining all of your physical disk partitions into one blob, and then
dividing that up between the logical volumes.

On this PC, according to `sudo pvdisplay`, I have a single PV (`/dev/nvme0n1p3`) of ~1.8TiB, in a single VG
(`ubuntu-vg`). Of that, according to `sudo vgdisplay`, 100GiB is allocated and ~1.7TiB is free.

So, I guess, I need to create a new LV and allocate some space to it:

```
sudo lvcreate -l 100%FREE -n emby-data-lv ubuntu-vg
```

That allocates all of the remaining space to the logical volume. If I start running out of space on the root fs, I can
probably reclaim the space later.

Then it needs a filesystem:

```
sudo mkfs.ext4 /dev/mapper/ubuntu--vg-emby--data--lv
```

Then it needs mounting somewhere:

```
sudo mkdir -p /mounts/emby-data
sudo mount /dev/mapper/ubuntu--vg-emby--data--lv /mounts/emby-data
```

Don't forget to add it to `/etc/fstab` so that it's mounted automatically at startup. Something like this should do the
trick:

```
/dev/disk/by-id/dm-name-ubuntu--vg-emby--data--lv /mounts/emby-data ext4 defaults 0 2
```

## Aside: HDHomeRun

I took the opportunity to upgrade the firmware on the HDHomeRun and to refresh the channel lineup.

## Installation

Download from <https://emby.media/linux-server.html>:

```
mkdir -p ~/Downloads
cd ~/Downloads
wget https://github.com/MediaBrowser/Emby.Releases/releases/download/4.7.6.0/emby-server-deb_4.7.6.0_amd64.deb
sudo dpkg -i emby-server-deb_4.7.6.0_amd64.deb
```

## Setup

Browse to <http://roger-nuc3:8096/>.

At least initially, I'm not going to try to migrate the settings from the previous installation. I don't think there's
anything of value in there, and the documentation for migrating server configuration doesn't fill me with confidence.

So I'm going to run through the setup wizard.

- Display language: English (UK).
- First user: `roger`, password in Bitwarden.
- Media Libraries: ignore this for now.
- Preferred Metadata Language: English, United Kingdom.
- Configure remote access: DISABLE automatic port mapping.
  - If I want to access Emby remotely, I'll use a VPN.
- Accept the terms of service.
- Click Finish.

## Settings

### Users

- New User / `tv` / Save.
- Allow media deletion from `Recordings`.

### Emby Premiere

To use Live TV, you need Emby Premiere. I've already paid for it, and the key is in my email archive.

On the settings page, under Server / Emby Premiere, enter the key code and press Save. You'll need to restart the
server, so:

```
sudo service emby-server restart
```

### Live TV

- Add TV Source / HD Homerun / Detect My Devices.
- Add Guide Data / United Kingdom / Emby Guide Data / Post Code: XX / Lineup: Freeview (London).

```
sudo mkdir -p /mounts/emby-data/recordings
sudo chown -R emby.emby /mounts/emby-data/recordings
```

- Advanced / Default recording path: `/mounts/emby-data/recordings`
- Advanced / Stop 3 minutes after.
