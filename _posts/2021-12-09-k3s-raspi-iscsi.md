---
title: "k3s on Raspberry Pi: iSCSI"
date: 2021-12-09T19:42:00Z
layout: series
series: k3s
tags: k3s raspberry-pi iscsi
---

The default option for persistent volumes on k3s is `local-storage`, which provisions (on-demand) the storage on the node's local disk. This has the unfortunate side-effect that the container is now tied to that particular node.

To get around this, I thought I'd take a look at iSCSI. I've got an unused Synology DS211 NAS that can act as an iSCSI target, so I put a disk in it, installed the latest DSM version and got started.

I mean: how hard could it be?

## Setting up the iSCSI Target

Setting up the iSCSI target is relatively simple:

1. Log into the DS211.
2. Open the main menu and choose "iSCSI Manager".
3. On the "Target" page, click "Create".
4. Give it a sensible name. Since I'm messing around with Docker, I called it "docker-registry". I also edited the IQN, replacing "Target-1" with "docker-registry".
5. I did not enable CHAP. This is all on a local, trusted, network, and I didn't want to deal with auth at this point.
6. Click "Next".
7. Select "Create a new iSCSI LUN". A LUN (Logical Unit Number) is just a fancy name for a volume, effectively.
8. It needs a name, I named it "docker-registry-LUN-1".
   - It seems like you can have multiple LUNs per target, but I've not tried that. Presumably it allows you to expand the disk later.
9. I've only got one disk (and one volume) in my DS211, so the default location is the only option.
10. I opted for a capacity of 64GB. It's probably overkill for a toy cluster, but I've got plenty of space, so meh.
11. You can choose between "Thick" and "Thin" provisioning.
    - The help text is a little vague about this: "better performance" vs. "flexible storage allocation", but what I think it means is: "pre-allocated" vs. "allocated on demand".
    - The first will take a chunk of space on the NAS, even if you're not using all of the space inside the LUN.
    - The second only grows when the LUN grows, but could fail (catastrophically?) if you run out of space on the NAS.

Note that a LUN can only be used by [one initiator at a time](https://serverfault.com/questions/976116/iscsi-multiple-initiators-for-the-same-lun). There are cluster-aware filesystems that get around this with fancy locking or other schemes, but that's out of scope here.

See also:
- [Synology KB: How to start using the iSCSI target service on Synology NAS](https://kb.synology.com/en-vn/DSM/tutorial/How_to_use_the_iSCSI_Target_service_on_Synology_NAS)
- [TechRepublic: How to integrate a Synology NAS in your VMware Lab](https://www.techrepublic.com/article/how-to-integrate-a-synolgy-nas-in-your-vmware-lab/)
- [ServeTheHome: How to Setup an iSCSI Target Using a Synology DS1812+ NAS](https://www.servethehome.com/setup-iscsi-target-synology-ds1812-nas/)


## Install the `open-iscsi` package on your cluster nodes

```bash
sudo apt install open-iscsi    # on all cluster nodes
```

I didn't do this until later, and I think it caused me some problems.

Aside: you can probably use node labels and selectors so that iSCSI-using containers are only scheduled on iSCSI-having nodes.

## Mount the volume

In the deployment, I've got the following (massively abridged):

```yaml
apiVersion: apps/v1
kind: Deployment
spec:
  template:
    spec:
      containers:
      - name: registry
        image: registry:2
        volumeMounts:
        - name: registry-vol
          mountPath: "/var/lib/registry"
        # ...
      volumes:
      - name: registry-vol
        iscsi:
          targetPortal: 192.168.28.124:3260
          iqn: iqn.2000-01.com.synology:ds211.docker-registry.25e6c0dc53
          lun: 1
          readOnly: false
# ...
```

Note the IP address for the `targetPortal`. This is **important** (and also annoying). See [rancher#12433](https://github.com/rancher/rancher/issues/12433).

I added the volume to the deployment, as [this page](https://www.michaelburch.net/blog/Kubernetes-on-Raspberry-Pi-with-K3s.html) suggests, but I suspect -- based on the [k8s docs](https://kubernetes.io/docs/concepts/storage/persistent-volumes/#types-of-persistent-volumes) -- that you can use a persistent volume (PV) and persistent volume claim (PVC) instead.

I don't know why you'd choose one over the other at this point.

## So it worked then?

No. At this point, I ran into a bunch of problems.

The first one is that I hadn't installed the `open-iscsi` package yet. I'm so used to everything just retrying that I got a bit careless about doing things in the "right" order. I don't know whether this was the cause of my later problems, but ... maybe?

The main problem was that my container refused tomount the volume. It kept reporting the following:

```
iscsiadm: initiator reported error (19-encountered non-retryable iSCSI login failure)
```

Running `iscsid` [in debug mode](https://www.thegeekdiary.com/how-to-troubleshoot-iscsi-issue-is-centos-rhel-67/) gave me a little more:

```
iscsid: conn 0 login rejected: initiator error - target not found (02/03)
```

...but ultimately I have no idea what was wrong. Eventually, I created a 1GB volume on the NAS and attempted to mount it on the node, rather than in a container:

```bash
$ sudo iscsiadm -m discovery -t sendtargets -p 192.168.28.124:3260
...
192.168.28.124:3260,1 iqn.2000-01.com.synology:ds211.docker-registry.25e6c0dc53
192.168.28.124:3260,1 iqn.2000-01.com.synology:ds211.tmp.25e6c0dc53
...

$ sudo iscsiadm -m node \
    --targetname iqn.2000-01.com.synology:ds211.tmp.25e6c0dc53 \
    --portal 192.168.28.124:3260 --login
```

At that point, it all started working and the container was able to mount the volume correctly. ¯\\\_(ツ)\_/¯

## Is the volume persistent?

Yes.

I logged into the container:

```
$ kubectl --namespace docker-registry exec --stdin --tty docker-registry-7698dbd756-bnggx -- /bin/sh
# touch /var/lib/registry/hello-world
```

Then I deleted the pod and waited for the deployment to recreate it (on a different node). The file was still there.

## Can I use iSCSI on the node?

Yeah, it's just standard Linux stuff. Once you've logged in...

```
$ sudo iscsiadm -m node \
    --targetname iqn.2000-01.com.synology:ds211.tmp.25e6c0dc53 \
    --portal 192.168.28.124:3260 --login
```

...you have a new block device...

```
$ lsblk
NAME   MAJ:MIN RM  SIZE RO TYPE MOUNTPOINT
sda      8:0    1 57.3G  0 disk
├─sda1   8:1    1  256M  0 part /boot
└─sda2   8:2    1 57.1G  0 part /
sdb      8:16   0    1G  0 disk
```

`sdb` is the "tmp" volume. At this point you can partition, format, and mount it as normal.
