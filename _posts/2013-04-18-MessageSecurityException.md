---
layout: post
title: "MessageSecurityException"
date: 2013-04-18T14:59:48.566Z
tags: wcf
alias: /post/UXALmiksGSwhAAAB/messagesecurityexception
---

As I run across them, I'm going to update this page with various causes of the WCF `MessageSecurityException`.

The HTTP request was forbidden with client authentication scheme 'Anonymous'; The remote server returned an error: (403) Forbidden.
--

    System.ServiceModel.Security.MessageSecurityException: The HTTP request was forbidden with client authentication scheme 'Anonymous'.
    ---> System.Net.WebException: The remote server returned an error: (403) Forbidden.
    
I have security turned off, so I'm not expecting to see anything about authentication,
but what WCF is attempting to tell me is that we're *not* using any security.

In this particular case, I was accessing the URL as http://server.domain.local/Foo/Bar/,
but it was binding to http://localhost/Foo/Bar/, and I had HostNameMatch="Exact".

The clue was in the "(403) Forbidden".
