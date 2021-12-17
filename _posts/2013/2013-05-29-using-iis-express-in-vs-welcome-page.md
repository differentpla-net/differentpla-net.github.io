---
title: Using IIS Express in vs-welcome-page
date: 2013-05-29T17:35:18Z
---
So, why did I use *IIS Express* to implement my *vs-welcome-page* extension?
I'm using Nancy, so why didn't I just use the self-hosting support and host it
inside Visual Studio instead?

Because of a misunderstanding about URL ACLs. The HTTP stack in Windows allows
you to bind multiple processes to the same port number, as long as they use
different URL "prefixes" (namespaces, if you like).

However, in order to prevent malicious processes binding
to well-known URLs, Windows has the concept of URL ACLs, which control which user
accounts can bind to while prefixes.

Previously, when I've used Nancy, and I've attempted to listen on an address as follows:

    const string prefix = "http://localhost:10250/";
    var host = new NancyHost(new Uri(prefix));
    host.Start();

...I've received an "Access Denied" exception. When using *IIS Express*, I've not
seen this. I just guessed that *IIS Express* had some magic in it that allowed it
to subvert the rules, and (since this was a weekend project) I didn't
bother investigating any further.

Today, however, I had an opportunity to investigate this. I started with `HttpListener`:

	const string prefix = "http://localhost:10250/";
    var listener = new HttpListener();
    listener.Prefixes.Add(prefix);
    listener.Start();

... and this works fine. It appears that you don't need URL ACLs to bind to
"localhost", and that this is the magic that *IIS Express* is using.

So why doesn't it work with *Nancy*? It turns out that I was missing one thing:
in the self-hosting code, Nancy transforms "localhost" into "+" (meaning all available addresses),
which then falls under the purview of the URL ACL mechanism.

Fortunately, this behaviour can be controlled by the `HostConfiguration.RewriteLocalhost`
flag passed to the `NancyHost` constructor:

    var configuration = new HostConfiguration { RewriteLocalhost = false };
    var host = new NancyHost(configuration, new Uri(prefix));
    host.Start();

... and now it all works fine. I'll be updating my VS extension in the next few
days to remove the dependency on *IIS Express*, which will reduce the number of
moving parts, simplify the packaging process, and make it easier to enable some
other features that I've been thinking about.
