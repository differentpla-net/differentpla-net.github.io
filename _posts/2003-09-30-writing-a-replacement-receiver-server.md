---
title: "Writing a Replacement Receiver Server"
date: 2003-09-30T23:01:00.000Z
x-drupal-nid: 109
x-needs-review: 2003-09-30T23:01:00.000Z
tags: empeg rio-receiver
---
## Introduction

While I was at empeg, I wrote the original server software for the Rio Receiver. It has, shall we say, certain limitations. Some of them were down to the way marketing wanted it to work, and some of them are squarely my fault.

To atone for my sins, and to help learn something new, I'm going to try to rewrite it in C#, using .NET. At the time of this writing, I'm currently "between jobs", so I'll hopefully have plenty of time. I'm currently looking for another job, so I may not actually finish it.

I should note that, since I no longer work for Rio, and no longer have access to the original source code, this is a kind of "clean room" implementation. I'll be reverse-engineering the protocol by looking at network traces.

Even if I don't finish, hopefully some of the information here will be useful to others attempting to write replacement servers or clients for the Rio Receiver.

## Objectives

*   It must run as a Windows service. Each of the different components should be individually controllable as Windows services.
*   It must be able to support on-the-fly database updates.
*   It should use C# and .NET. This has implications for the installer.
*   It should use MSDE as the database backend. This also has implications for the installer.

## Architecture

The original Rio Receiver server comprises a DHCP server, an NFS server and an HTTP server. There's also some UDP control/status stuff going on. The Receiver finds the NFS server and HTTP server using a UDP broadcast, so we'll need to support this as well. There's also a database of the music, and something to update that database.

## The HTTP server

The easiest thing to write is going to be the webserver, so armed with a copy of RFC2616, I quickly hacked something simple together. You can find the source code [here](/node/view/46). Currently, this just echoes the client request. I'll be adding the other features required by the Rio Receiver as I figure them out.

## Getting started

The first thing that I'll need to do is reverse engineer the network protocols. A copy of [Ethereal](http://www.ethereal.com/) should do nicely.

## DHCP

Without any server software installed, I fired up the Receiver and watched the network traffic with Ethereal. The first thing that the Receiver does is use DHCP to acquire an IP address. See [RFC 2131](http://www.ietf.org/rfc/rfc2131.txt) for more information. Since I already have a DHCP server on my network, I didn't need to do anything about this. I'll look into it later.

The Rio Receiver also supports 169.254.x.y [automatic IP addresses](http://www.upnp.org/download/draft-ietf-zeroconf-ipv4-linklocal-01-Apr.txt).

## NFS server discovery

The next thing that the client does is send out a UDP service discovery request. This is vaguely similar to an [SSDP request](http://www.upnp.org/download/draft_cai_ssdp_v1_03.txt). It's not the same (in particular it's on a different port) because the SSDP spec wasn't complete when we wrote the original software.

This UDP request is a subnet-local broadcast (e.g. to 192.168.0.255) with source and destination port numbers both set to 21075\. It contains the string "upnp:uuid:1D274DB1-F053-11d3-BF72-0050DA689B2F" followed by a linefeed, followed by the MAC address of the Receiver, followed by a linefeed.

The response is sent to the IP address of the Receiver, also on port 21075, and contains the IP address of the NFS server. This is formatted as a URL to an XML file, for example "http://192.168.0.2/something.xml". There is no webserver at this location, and that file doesn't exist.

This address refers to the portmap daemon. If the portmapper is running on a non-standard port (i.e. other than 111), this should be specified in the response. The included server software runs its portmapper on port 21076, and would respond with, e.g. "http://192.168.0.3:21076/descriptor.xml".

Because I don't particularly fancy implementing an NFS server in C# just yet, I'm going to use my Linux box as the [NFS server](/node/view/14).

## Multiple Services in the Same Binary

Windows supports multiple services sharing the same binary. This is how I'm going to structure the new Audio Receiver server. This ought to make it easier for the various services to register with the Service Discovery Service.

The Audio Receiver server has the following services in the same binary:

*   DHCP Server
*   Service Discovery
*   NFS Server
*   HTTP Server

The NFS and HTTP services depend on the Service Discovery service.
You can find source for this [here](/node/view/45). This assumes that you've got a copy of [the SC.EXE](http://msdn.microsoft.com/archive/default.asp?url=/archive/en-us/dnarpic/html/msdn_scmslite.asp) utility. I've written some batch files to make installation easier..

## HTTP server discovery

This proceeds in the same way as for NFS server discovery, but with "upnp:uuid:1D274DB0-F053-11d3-BF72-0050DA689B2F" in the request.

## /tags

The first thing that the Receiver asks for from the HTTP server is `/tags`. This is a list of the field names supported by the server's database, each terminated with LF (0x0A). The protocol used by the Receiver is quite closely based on that used by the car player, and this is effectively the `/empeg/var/tags` file.

For now, we'll just return a hard-coded list.

## /layout

The next thing that the Receiver does is ask for its [layout definitions](/node/view/12). In the stock server, these are stored as files in the webserver's directory. For now, we'll go and grab them from the location where the stock server has them installed, by poking around in the registry.

* * *

Update: I've since found gainful employment, so this is on hold. For more info, you might want to check out [this thread](http://empeg.comms.net/php/showthreaded.php?Cat=&Board=hackers_prog&Number=181804) on the empeg BBS.
