---
title: "Streaming HTTP responses in .NET"
date: 2012-07-14T19:11:10.000Z
redirect_from: /content/2012/07/streaming-http-responses-net
---
Sometimes you have a web service or web application, and, for one request, you need to transfer an unknown (and potentially unbounded) amount of data in the response.

In this post, I'll show some code for doing this with various parts of the .NET stack.

## A brief history of HTTP

First, however, a diversion. How does this actually work under the hood?

HTTP is implemented on top of TCP/IP, and TCP/IP already supports streaming of data. You write something; the client reads something; you write some more; the client reads some more. Brilliant. This is streaming.

Back in the early days of HTTP, this was perfect. A client would open a connection, the server would write some data, and then the server would close the connection, flagging the end of the data. Awesome.

However, opening and closing connections takes time, so HTTP added persistent connections (flagged by `Connection: Keep-Alive`). The point of this is that the client can send more than one request and the server can send more than one response without closing the connection. But how does the client know when the data for a single response is finished?

Enter the `Content-Length` header. This header is added to the start (it's a header, after all) of the response. It tells the client how much data to expect in that response.

The problem with `Content-Length` is that the server needs to know, ahead of time, how much data is in the response. This is fine for static files (it can just ask the filesystem) and it's fine for small responses (it can buffer the response and calculate the size).

Unfortunately, it breaks streaming, because the server (as mentioned) needs to buffer the response to calculate the size. This means that the server can't start sending the response until the entire response has been generated.

There are two ways to solve this problem: (1) go back to the old `Connection: close` behaviour, or (2) use chunked encoding.

Chunked encoding (flagged by `Transfer-Encoding: chunked`) allows the server to send pieces of the response as they're ready, without needing to know the total length of the response up front.

This works by sending each piece (chunk) of the response separately, each with a length prefix. The end of the response is flagged by a zero-length chunk.

## HttpListener

On with the code.

So: using a basic HttpListener-based program, how can we implement this?

<pre>byte[] buffer = Encoding.UTF8.GetBytes("Hello World\n");
context.Response.ContentType = "text/plain"
context.Response.SendChunked = true;

for (int i = 0; i < 10; ++i)
{
    context.Response.OutputStream.Write(buffer, 0, buffer.Length);
    context.Response.OutputStream.Flush();
    Thread.Sleep(500);    // Difficult calculation goes here.
}
context.Response.Close();
</pre>

To get this to work, set `SendChunked = true`, and remember to `Flush()` the output stream after each chunk.

## ASP.NET IHttpHandler

More code:

<pre>    public class StreamingHandler : IHttpHandler
    {
        public void ProcessRequest(HttpContext context)
        {
            byte[] buffer = Encoding.UTF8.GetBytes("Hello World");
            context.Response.AddHeader("Connection", "close");
            context.Response.ContentType = "text/plain";
            context.Response.BufferOutput = false;

            for (int i = 0; i < 10; ++i)
            {
                context.Response.OutputStream.Write(buffer, 0, buffer.Length);
                context.Response.OutputStream.Flush();
                Thread.Sleep(500);    // Difficult calculation goes here.
            }

            context.Response.Close();
        }

        public bool IsReusable
        {
            get { return false; }
        }
    }
</pre>

This one's an `IHttpHandler`, which can be dropped into the ASP.NET pipeline by registering it in `Web.config`, as follows:

<pre>  <system.webServer>
    <handlers>
      <add name="Streaming-Handler" verb="*" path="*" type="web_streaming.StreamingHandler, web-streaming"/>
    </handlers>
  </system.webServer>
</pre>

## NancyFX

So: why am I writing this blog post?

Because I'm trying to get chunked encoding working in [Nancy](http://nancyfx.org/), and I can't do it without a (minor) change to Nancy. I think I've figured out why...

First, here's my code:

<pre>        public HomeModule()
        {
            Get["/slow"] = _ => new SlowStreamResponse();
        }

        private class SlowStreamResponse : Response
        {
            public SlowStreamResponse()
            {
                ContentType = "text/plain";
                Contents = s => {
                    byte[] bytes = Encoding.UTF8.GetBytes("Hello World\n");
                    for (int i = 0; i < 10; ++i)
                    {
                        s.Write(bytes, 0, bytes.Length);
                        s.Flush();
                        Thread.Sleep(500);
                    }
                };
            }
        }
</pre>

This is basically the same as the other two examples; it takes advantage of the fact that Nancy passes us the output stream directly: `response.Contents.Invoke(context.Response.OutputStream)`

Which means that we can `Write()` and `Flush()` to our heart's content. Unfortunately, it doesn't work, because we're missing one thing.

`OutputStream` is an instance of `HttpResponseStream`, which delegates to an `HttpWriter`. When you call `Flush()` on the stream, it does nothing.

The clever bit actually happens when you call `Write()`. This actually calls `HttpWriter.WriteFromStream`. If response buffering is off, this calls `HttpResponse.Flush`, which is where the chunked encoding magic happens.

So: how does response buffering get turned off? This is done when you set the `HttpResponse.BufferOutput` property, or when `HttpWriter` is constructed, which is all `internal`.

Unfortunately, Nancy doesn't provide a way to set this flag, meaning that we can't turn on chunked encoding.

I'm going to put together a patch and submit it.
