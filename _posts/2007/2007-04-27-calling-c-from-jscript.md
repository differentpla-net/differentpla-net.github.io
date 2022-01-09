---
title: "Calling C# from JScript"
date: 2007-04-27T09:03:18.000Z
---
It's possible, through the magic of COM interop, to call C# code from JScript or VBScript. Here's an example of how to do it from JScript.

First, you need to put together your C# class. You should define it with a separate interface, like this:

<pre>using System;
using System.Runtime.InteropServices;

namespace HelloWorld
{
    [ComVisible(true),
        Guid("6578E2DB-B8CE-4291-A617-A576E5E56ABE"),
        InterfaceType(ComInterfaceType.InterfaceIsDual)]
    interface IHelloWorld
    {
        string Message { get; }
    }

    [ComVisible(true),
        Guid("5067EE69-0248-4840-B81A-013F480725EC"),
        ProgId("HelloWorld.HelloWorld"),
        ClassInterface(ClassInterfaceType.None)]
    public class HelloWorld : IHelloWorld
    {
        public string Message
        {
            get { return "Hello World"; }
        }
    }
}</pre>

Then, you'll need to register the C# class so that COM can see it. You can use RegAsm.exe for this. Unfortunately, RegAsm doesn't always store the CodeBase (the full path to the DLL). The simplest way around this is to use `RegAsm /regfile:HelloWorld.reg` to create a .REG file. Then you can edit this to add the CodeBase entries. It should look like this:

<pre>REGEDIT4

[HKEY_CLASSES_ROOT\HelloWorld.HelloWorld]
@="HelloWorld.HelloWorld"

[HKEY_CLASSES_ROOT\HelloWorld.HelloWorld\CLSID]
@="{5067EE69-0248-4840-B81A-013F480725EC}"

[HKEY_CLASSES_ROOT\CLSID\{5067EE69-0248-4840-B81A-013F480725EC}]
@="HelloWorld.HelloWorld"

[HKEY_CLASSES_ROOT\CLSID\{5067EE69-0248-4840-B81A-013F480725EC}\InprocServer32]
@="mscoree.dll"
"ThreadingModel"="Both"
"Class"="HelloWorld.HelloWorld"
"Assembly"="HelloWorld, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null"
"RuntimeVersion"="v2.0.50727"
**"CodeBase"="D:\\Source\\Gadgets\\HelloWorld\\HelloWorld\\bin\\Debug\\HelloWorld.dll"**

[HKEY_CLASSES_ROOT\CLSID\{5067EE69-0248-4840-B81A-013F480725EC}\InprocServer32\1.0.0.0]
"Class"="HelloWorld.HelloWorld"
"Assembly"="HelloWorld, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null"
"RuntimeVersion"="v2.0.50727"
**"CodeBase"="D:\\Source\\Gadgets\\HelloWorld\\HelloWorld\\bin\\Debug\\HelloWorld.dll"**

[HKEY_CLASSES_ROOT\CLSID\{5067EE69-0248-4840-B81A-013F480725EC}\ProgId]
@="HelloWorld.HelloWorld"

[HKEY_CLASSES_ROOT\CLSID\{5067EE69-0248-4840-B81A-013F480725EC}\Implemented Categories\{62C8FE65-4EBB-45E7-B440-6E39B2CDBF29}]</pre>

I've marked the extra bits in bold.

Once you've used the .REG file to register the C# class, you can put together some JScript, like this:

<pre>var hw = new ActiveXObject("HelloWorld.HelloWorld");
WScript.Echo(hw.Message);</pre>

And you're done.
