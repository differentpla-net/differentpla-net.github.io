---
title: "Downloading from an HTTP Server using WinInet"
date: 2004-02-26T11:45:00.000Z
x-drupal-nid: 63
x-needs-review: 2004-02-26T11:45:00.000Z
---
The WinInet functions allow an application to interact with Gopher, FTP and HTTP servers. This article shows how to use the WinInet API to download from an HTTP server.

## InternetOpen

The first thing that the application needs to do is to initialise WinInet for use by that application:

<pre>LPCTSTR lpszAgent = "WinInetGet/0.1";
HINTERNET hInternet = InternetOpen(lpszAgent,
		INTERNET_OPEN_TYPE_PRECONFIG, NULL, NULL, 0);</pre>

The `InternetOpen` function allows the user to specify the proxy to be used. Here we tell it to use whatever the user has already configured in Internet Explorer. It also requires the "user agent" string, which identifies the application. Here, our demo application is going to be called WinInetGet, so that's what we pass, along with a version number.
When we're finished with the WinInet functions, we should remember to call `InternetCloseHandle`.

## InternetConnect

Having initialised the WinInet functions, the next thing we do is connect to a particular server:

<pre>LPCTSTR lpszServerName = "vague.home.differentpla.net";
INTERNET_PORT nServerPort = INTERNET_DEFAULT_HTTP_PORT;
LPCTSTR lpszUserName = NULL;
LPCTSTR lpszPassword = NULL;
DWORD dwConnectFlags = 0;
DWORD dwConnectContext = 0;
HINTERNET hConnect = InternetConnect(hInternet,
				lpszServerName, nServerPort,
				lpszUserName, lpszPassword,
				INTERNET_SERVICE_HTTP,
				dwConnectFlags, dwConnectContext);</pre>

You'll need to change the server name (since this one refers to my Linux test box at home). We're not passing a user name or password, nor are we passing any connection flags. The `dwConnectContext` is an application-defined value that's passed to the callback function registered with `InternetSetStatusCallback`. Since we're not using `InternetSetStatusCallback`, we pass zero.

## HttpOpenRequest

The application then needs to form an HTTP request. This is done with the `HttpOpenRequest` function:

<pre>LPCTSTR lpszVerb = "GET";
LPCTSTR lpszObjectName = "/";
LPCTSTR lpszVersion = NULL;			// Use default.
LPCTSTR lpszReferrer = NULL;		// No referrer.
LPCTSTR *lplpszAcceptTypes = NULL;	// Whatever the server wants to give us.
DWORD dwOpenRequestFlags = INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP |
		INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS |
		INTERNET_FLAG_KEEP_CONNECTION |
		INTERNET_FLAG_NO_AUTH |
		INTERNET_FLAG_NO_AUTO_REDIRECT |
		INTERNET_FLAG_NO_COOKIES |
		INTERNET_FLAG_NO_UI |
		INTERNET_FLAG_RELOAD;
DWORD dwOpenRequestContext = 0;
HINTERNET hRequest = HttpOpenRequest(hConnect, lpszVerb, lpszObjectName, lpszVersion,
		lpszReferrer, lplpszAcceptTypes,
		dwOpenRequestFlags, dwOpenRequestContext);
</pre>

The `HttpOpenRequest` function doesn't actually communicate with the server at this point. We use it to create the HTTP request object, which we can then fill in, before sending it.

The interesting thing (from the HTTP point of view) is that we specify "GET /" in the `lpszVerb` and `lpszObjectName` parameters. The flags are just a bunch of available flags that I thought looked interesting. We pass NULL as the referrer, since we didn't come here from another page. By passing NULL in the `lplpszAcceptTypes` parameter, we signal that we're not bothered about what we're given. Most servers will interpret this as "text/*". Again, we pass zero for the application-defined context value.

## HttpSendRequest

Since we're done putting together our HTTP request, we can send it to the server using the `HttpSendRequest` function:

<pre>BOOL bResult = HttpSendRequest(hRequest, NULL, 0, NULL, 0);</pre>

The last four parameters allow us to supply additional headers and any optional data. The optional data is generally only used in `POST` and `PUT` operations.
## HttpQueryInfo

After we've sent the request, and the response has come back, we ought to check the response headers. This is done with the `HttpQueryInfo` function:

<pre>DWORD dwInfoLevel = HTTP_QUERY_RAW_HEADERS_CRLF;
DWORD dwInfoBufferLength = 10;
BYTE *pInfoBuffer = (BYTE *)malloc(dwInfoBufferLength+1);
while (!HttpQueryInfo(hRequest, dwInfoLevel, pInfoBuffer, &dwInfoBufferLength, NULL))
{
	DWORD dwError = GetLastError();
	if (dwError == ERROR_INSUFFICIENT_BUFFER)
	{
		free(pInfoBuffer);
		pInfoBuffer = (BYTE *)malloc(dwInfoBufferLength+1);
	}
	else
	{
		fprintf(stderr, "HttpQueryInfo failed, error = %d (0x%x)\n",
			GetLastError(), GetLastError());
		break;
	}
}

pInfoBuffer[dwInfoBufferLength] = '\0';
printf("%s", pInfoBuffer);
free(pInfoBuffer);</pre>

In common with many other Windows API functions, `HttpQueryInfo` will tell you if you have not allocated enough buffer space for the result. This `while` loop is a good way to make sure that you get it right. Note that we add a null terminator, so we allow for this in our call to `malloc`. The MSDN documentation implies that the string will already be zero-terminated, but it's a little ambiguous.

Note that we're deliberately not allocating enough space. This is so that we can test the logic in the `while` loop. In a real application, we'd allocate a larger buffer, to avoid calling `HttpQueryInfo` multiple times.

The `HttpQueryInfo` function can return a specific header value from the HTTP response, e.g. pass HTTP_QUERY_DATE to retrieve the "Date:" header. For custom header values, the application can pass HTTP_QUERY_CUSTOM and pass the name of the header in the buffer. It will be overwritten with the header value. We pass HTTP_QUERY_RAW_HEADERS_CRLF, because we're not particularly interested in the actual content; this is just a demo application.

## InternetReadFile

To retrieve the entity body from the HTTP response, we'll use a loop like this:

<pre>DWORD dwBytesAvailable;
while (InternetQueryDataAvailable(hRequest, &dwBytesAvailable, 0, 0))
{
	BYTE *pMessageBody = (BYTE *)malloc(dwBytesAvailable+1);

	DWORD dwBytesRead;
	BOOL bResult = InternetReadFile(hRequest, pMessageBody,
				dwBytesAvailable, &dwBytesRead);
	if (!bResult)
	{
		fprintf(stderr, "InternetReadFile failed, error = %d (0x%x)\n",
			GetLastError(), GetLastError());
		break;
	}

	if (dwBytesRead == 0)
		break;	// End of File.

	pMessageBody[dwBytesRead] = '\0';
	printf("%s", pMessageBody);
	free(pMessageBody);
}</pre>

Note that the `InternetQueryDataAvailable` function blocks until data is available or an error occurs.
Full source code for this article is [here](/node/view/130).