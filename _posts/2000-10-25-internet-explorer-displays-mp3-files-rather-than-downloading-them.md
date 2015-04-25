---
title: "Internet Explorer displays MP3 files, rather than downloading them"
date: 2000-10-25T23:00:00.000Z
x-drupal-nid: 112
x-needs-review: 2000-10-25T23:00:00.000Z
---
This one's very rare. I don't anticipate it ever happening to anyone else. It did, however, happen to me, so I thought I'd share it.

Normally, clicking on an MP3 file causes Internet Explorer to display a dialog box, asking whether you want to save the file, or open it directly. In this case, it wasn't working. The file was just getting displayed in the browser window.

Some snooping around in the relevant parts of the registry identified the problem. There was a little bit of a snafu in my MIME type database.

To briefly explain, the HTTP response (see RFC 2616 for details) contains a "Content-Type" header, which describes the MIME type of the data being sent.

This header is used to locate the "helper application" (to use Netscape parlance) responsible for rendering the content. In Windows, this information is stored in the "HKEY_CLASSES_ROOT\MIME\Database\Content Type" registry key.

I've not found out what all of the settings in here do yet, but it was this portion of my registry that had the snafu. There was a missing CLSID entry in the "audio/mpeg" key. I copied the one from the "audio/midi" key, and was back in business.