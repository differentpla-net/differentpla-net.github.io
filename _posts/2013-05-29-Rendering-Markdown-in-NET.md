---
layout: post
title: "Rendering Markdown in .NET"
date: 2013-05-29T17:19:57.678Z
tags: vs-welcome-page nancy markdown
alias: /post/UaY5DQFqM2ZBAAAB/rendering-markdown-in-net
---

There are several options for rendering Markdown to HTML in .NET. If you bring
up the NuGet Package Manager dialog and search for "markdown", you get three
pages of results, including `MarkdownSharp`, which is the renderer used by
Stack Overflow, so it must be good.

More intriguingly, however, is `Kiwi.Markdown` which mentions support for
GitHub-flavoured markdown and GitHub wiki pages. This sounds ideal, so we'll
quickly play with that.

At this point, we need is a simple web application that we can host in IIS Express.
We need to be able to provide it with a path containing a `README.md` file.

Spike: Rendering Markdown
--

I'm going to use **Nancy** for this, for two reasons:

1. I think it'll be lighter-weight than a full-blown ASP.NET MVC 4 application.
2. I really like it.

So, the first thing we need is an "ASP.NET Empty Web Application". I'll call it
"StartPage.WebApplication". This gives us a C# web project that contains nothing
more than a `Web.config` file.

To this, we add the `Nancy.Hosting.Aspnet` NuGet package, and add the home page:

    public class HomeModule : NancyModule
    {
        public HomeModule()
        {
            Get["/"] = _ => "Hello World";
        }
    }


At this point, I double-check that it works in IIS Express if I run it as we're
planning to:

    Start-Process $iisexpress `
	  ('/path:"{0}"' -f 'C:\Usersoger\Source\vs-start-page\StartPage.WebApplication'), '/port:15510' `
	  -WindowStyle Hidden

Yep. That's all good.

Next up is actually rendering something, so we'll add the `Kiwi.Markdown` package
to the project, and add some code to use it:

	Get["/"] = context =>
	    {
	        var contentProvider = new FileContentProvider(@"C:\Usersoger\Source\vs-start-page");
	        var converter = new MarkdownService(contentProvider);
	        var document = converter.GetDocument("README");
	        return document.Content;
	    };

Cool. That works. `MarkdownService` outputs an HTML snippet, rather than a full
document, assuming that you'll include it in something, but IE renders it fine,
so we can call that a success.

*Note:* It turns out that `Kiwi.Markdown` uses `MarkdownSharp` anyway.
