---
title: "Discovering empegs on the Network Using C#"
date: 2004-03-19T10:00:00.000Z
tags: empeg
---
I've been playing around with learning C#. Today, I thought I'd write a C# program to find empegs on the local network.

It uses `Socket.BeginReceiveFrom` and `Socket.EndReceiveFrom`.

```c#
using System;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;

namespace FindEmpeg
{
  class EmpegFinder
  {
    class StateObject
    {
      public Socket socket;
      public byte []responseBytes;
      public const int RESPONSE_BUFFER_SIZE = 1024;

      public StateObject(Socket socket)
      {
        this.socket = socket;
        this.responseBytes = new byte[RESPONSE_BUFFER_SIZE];
      }
    };

    [STAThread]
    static void Main(string[] args)
    {
      // To find an empeg, we do a UDP broadcast on
      // port 8300 with a single question mark in it.
      IPEndPoint sourceEndPoint = new IPEndPoint(IPAddress.Any,
                                                      8300);
      IPEndPoint destinationEndPoint = new IPEndPoint(IPAddress.Broadcast,
                                                      8300);

      byte[] requestBytes = Encoding.ASCII.GetBytes("?");

      try
      {
        Socket s = new Socket(sourceEndPoint.AddressFamily,
                              SocketType.Dgram, ProtocolType.Udp);
        s.Bind(sourceEndPoint);
        s.SetSocketOption(SocketOptionLevel.Socket,
                              SocketOptionName.Broadcast, 1);

        Console.WriteLine("Looking for empegs on {0}...",
                              destinationEndPoint.ToString());
        s.SendTo(requestBytes, destinationEndPoint);

        EndPoint remoteEndPoint = new IPEndPoint(IPAddress.Any, 0);

        StateObject so = new StateObject(s);
        s.BeginReceiveFrom(so.responseBytes, 0,
                              StateObject.RESPONSE_BUFFER_SIZE, 0,
                              ref remoteEndPoint,
                              new AsyncCallback(ResponseCallback), so);

        // Then wait for 5s.
        Thread.Sleep(5000);
      }
      catch (SocketException e)
      {
        Console.WriteLine("SocketException: {0} ({1})",
                              e.ToString(), e.ErrorCode);
      }
      catch (Exception e)
      {
        Console.WriteLine("Exception: {0}",
                              e.ToString());
      }

    }

    public static void ResponseCallback(IAsyncResult ar)
    {
      try
      {
        StateObject so = (StateObject)ar.AsyncState;
        Socket client = so.socket;

        EndPoint remoteEndPoint = new IPEndPoint(IPAddress.Any, 0);
        int responseBytesReceived = client.EndReceiveFrom(ar,
                              ref remoteEndPoint);

        Console.WriteLine("Received response from {0}",
                              remoteEndPoint.ToString());

        // The received data is in the buffer originally passed.
        // So, we can either hand it over in the state object and
        // look at it here; or we can set an event and
        // release the other thread to look at it.
        // We'll do it in the response object, because
        // it makes it a little easier.
        string responseString = Encoding.ASCII.GetString(so.responseBytes,
                                    0, responseBytesReceived);

        Console.WriteLine("Response contains: {0}",
                                    responseString.ToString());

        // Issue another BeginReceiveFrom
        client.BeginReceiveFrom(so.responseBytes, 0,
                                    StateObject.RESPONSE_BUFFER_SIZE, 0,
                                    ref remoteEndPoint,
                                    new AsyncCallback(ResponseCallback),
                                    so);
      }
      catch (Exception e)
      {
        Console.WriteLine("Exception in ResponseCallback: {0}",
                              e.ToString());
      }
    }
  }
}
```
