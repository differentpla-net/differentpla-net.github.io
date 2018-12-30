---
title: "Returning HTML from a WCF service"
date: 2010-06-20T13:05:10.000Z
redirect_from: /content/2010/06/returning-html-wcf-service
---
I’m messing around with writing yet another a blog engine, as a way to learn ASP.NET MVC. One of the things that I’d like to do is have it support uploading from Windows Live Writer. This means that it needs to support XML-RPC and [Really Simple Discovery](http://cyber.law.harvard.edu/blogs/gems/tech/rsd.html) (RSD). More on the XML-RPC stuff later.

RSD is actually two-part. The URL that you provide to Live Writer is typically the index page of the blog. Live Writer expects to find a <link rel=”EditURI” type=”application/rsd+xml” …/> entry in the <head>.

Since I’m spiking my XML-RPC solution in a console WCF application, I also need to support an index page and an RSD page. This essentially requires returning HTML from a WCF service.

It’s quite easy:

<pre>ServiceHost discoveryServiceHost = new ServiceHost(typeof(ReallySimpleDiscovery));
Uri discoveryAddress = baseUri;
var discoveryEndpoint = discoveryServiceHost.AddServiceEndpoint(typeof(IReallySimpleDiscovery), new WebHttpBinding(WebHttpSecurityMode.None), discoveryAddress);
discoveryEndpoint.Behaviors.Add(new WebHttpBehavior());
discoveryServiceHost.Open();</pre>

IReallySimpleDiscovery is defined as follows:

<pre>[ServiceContract]
public interface IReallySimpleDiscovery
{
	[OperationContract, WebGet(UriTemplate = "/")]
	Stream Index();

	[OperationContract, WebGet(UriTemplate = "/rsd.xml", ResponseFormat = WebMessageFormat.Xml)]
	ReallySimpleDiscoveryFormatter Discover();
}</pre>

Index is implemented as follows:

<pre>internal class ReallySimpleDiscovery : IReallySimpleDiscovery
{
	public Stream Index()
	{
		string html = "...";

		// See http://blogs.msdn.com/b/justinjsmith/archive/2007/08/22/setting-http-headers-in-wcf-net-3-5.aspx
		if (WebOperationContext.Current != null)
			WebOperationContext.Current.OutgoingResponse.ContentType = "text/html";

		byte[] htmlBytes = Encoding.UTF8.GetBytes(html);
		return new MemoryStream(htmlBytes);
	}</pre>

Obviously, I need to implement ReallySimpleDiscoveryFormatter, but this should be enough to show that you can return plain HTML from a WCF service. If you really need to.
