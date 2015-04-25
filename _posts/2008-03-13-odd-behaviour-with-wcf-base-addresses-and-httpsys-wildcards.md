---
title: "Odd behaviour with WCF base addresses and HTTP.SYS wildcards"
date: 2008-03-13T21:30:10.000Z
x-drupal-nid: 215
x-needs-review: 2008-03-13T21:30:10.000Z
---
I'm at [DevWeek](http://www.devweek.com/) this week, and I went (among other things) to a couple of WCF presentations by Aaron Skonnard.

So, anyway, last night I put together a really simple WCF service. It looks like this:

<pre>using System;
using System.ServiceModel;

namespace WcfHost
{
    [ServiceContract]
    internal interface IHello
    {
        [OperationContract]
        string SayHello();
    }

    internal class HelloService : IHello
    {
        public string SayHello()
        {
            return "Hello";
        }
    }

    class Program
    {
        static void Main()
        {
            ServiceHost host = new ServiceHost(typeof(HelloService), new Uri("http://+:8010/"));
            host.AddServiceEndpoint(typeof(IHello), new BasicHttpBinding(), "Hello");
            host.Open();
            Console.WriteLine("Ready. Press Enter to quit.");
            Console.ReadLine();
            host.Close();
        }
    }
}</pre>

Note how I'm using the HTTP.SYS wildcard syntax for the URI. Unfortunately, WCF doesn't like that, and I get a UriFormatException: <tt>Invalid URI: The hostname could not be parsed.</tt>

So I change it to http://localhost:8080/, and instead I get another exception: <tt>HTTP could not register URL http://+:8080/. Your process does not have access rights to this namespace (see http://go.microsoft.com/fwlink/?LinkId=70353 for details).</tt>

It's not lying: I don't have access rights to the HTTP namespace (I run Windows Vista with UAC turned off, under a normal user account). Most things work fine. Occasionally I have to step out to the Administrator account for some stuff. In this case, that would be a <tt>netsh http add urlacl url=http://+:8080/ user=HOME\roger</tt>.

That's not what's bothering me.

What's bothering me is that WCF appears to have quietly translated my <tt>localhost</tt> to <tt>+</tt>, meaning that even though I've asked only to bind to 127.0.0.1, I've actually bound to all available addresses.

Surely this increases the attack surface for my WCF host? Or am I missing something?