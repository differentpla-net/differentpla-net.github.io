---
title: "Generating a Proxy/Stub DLL for an interface, the sane way"
date: 2004-01-29T10:01:00.000Z
---
Note: This article applies to Visual C++ 6\. There might be an easier way to do this in Visual Studio .NET

It seems that the only way to get some boilerplate IDL generated is to use the ATL wizard. Unfortunately, this requires that you create an object that will implement the interface at the same time. This is not necessarily what you want.

Recently, I was experimenting with sending a custom object between processes using Uniform Data Transfer (drag-and-drop or the clipboard), and needed to design an interface for this object. This time, I managed to separate the interface marshalling (the Proxy/Stub DLL) from the implementation. It's not that hard. You'll need the following files.

## my_interface.dsp/.dsw

The first thing that you'll need is a project file. Start with a "Win32 Dynamic-Link Library" project. In this example, we're calling it `my_interface`. Make sure that you select "An empty DLL project" from the wizard.

## my_interface.idl

```midl
import "oaidl.idl";
import "ocidl.idl";

[ object, uuid(12345678-9ABC-DEF0-1234-56789ABCDEF0),
  helpstring("IMyInterface Interface"),
  pointer_default(unique) ]
interface IMyInterface : IUnknown
{
    // methods go here
};
```

Add this file to the project. Open the settings dialog and go to the "MIDL" tab. Visual Studio should have automatically added it. Select your IDL file.

![](/images/2004/2004-01-29-generating-a-proxystub-dll-for-an-interface-the-sane-way/idl_proxy_midl.png)

In the "Output file name" box, put the name of the .tlb file that you'd like to generate. In this case, it shouldn't particularly matter, because we don't have a "library" section in our IDL file. However, if we have a name in this box, Visual Studio fails to work out its dependencies correctly, and builds things in the wrong order. Unless you _do_ have a "library" section, it's probably best to leave this empty.

In the "Output header file name" box, put `my_interface.h`. This is the name of the file from which both the client and server C++ code will get the interface definition.

You can check the "Stubless Proxies" box if you're targetting Windows 95 or newer, or Windows NT 4 or newer. This is almost certainly true these days. It's equivalent to the `/Oicf` switch.

In the "UUID File" box, put `my_interface_i.c`. This is the name of the file where all of the IID definitions will be stored.

Uncheck the "MkTypLib compatible" box.

Compile your project. You should have a successfully compiled IDL file. This will result in the generation of the following files:

*   dlldata.c
*   my_interface.h
*   my_interface_i.c
*   my_interface_p.c

Add them all to your project.
In this state, your project won't build, so you'll need to do a couple of other things.

First, add `_WINNT_WIN32=0x0400` to the preprocessor definitions. You'll need this to use the "Stubless Proxies" setting, above.

Next, add the following libraries to the Link tab:

*   rpcndr.lib
*   rpcns4.lib
*   rpcrt4.lib

Finally, add `REGISTER_PROXY_DLL` to the preprocessor definitions. You'll need this if you want `regsvr32.exe` to work with your DLL.

Your project should now compile and link.

## Registering the DLL

The AppWizard-generated COM DLLs are automatically registered at the end of the build process. To add this, go to the "Custom Build" tab for the project, and enter the following information:

![](/images/2004/2004-01-29-generating-a-proxystub-dll-for-an-interface-the-sane-way/idl_proxy_regsvr32.png)

Done. Source is here: [my_interface.zip](/drupal-4.7.3/my_interface.zip).

Late note: You might need to add a .def file to the project. I'm not sure why. It should look like this:

    ; my_interface.def : Declares the module parameters.

    LIBRARY      "my_interface.DLL"

    EXPORTS
    	DllGetClassObject       @1	PRIVATE
    	DllCanUnloadNow         @2	PRIVATE
    	GetProxyDllInfo         @3	PRIVATE
    	DllRegisterServer		@4	PRIVATE
    	DllUnregisterServer		@5	PRIVATE
