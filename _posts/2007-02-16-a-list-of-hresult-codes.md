---
title: "A list of HRESULT codes"
date: 2007-02-16T10:50:44.000Z
x-drupal-nid: 98
x-needs-review: 2007-02-16T10:50:44.000Z
---
...because every time I search for a given HRESULT, all I get is yet another frigging VB website regurgitating the contents of WinError.h

<dl>
<dt>ERROR_BAD_IMPERSONATION_LEVEL (1364 or 0x80070542)</dt>

<dd>Usually means that you've forgotten to call `CoInitializeSecurity` or `CoSetProxyBlanket` with the `RPC_C_IMP_LEVEL_IMPERSONATE` parameter.
 Oddly, I've seen `CoImpersonateClient` succeed, but a later call to `RegOpenKeyEx` fail with this error. I'd have been expecting ERROR_ACCESS_DENIED myself.</dd>

<dt>E_NOINTERFACE (0x80004002)</dt>

<dd>When attempting to marshal an interface pointer, COM couldn't figure out how to create the proxy and stub. Check under `HKEY_CLASSES_ROOT\Interface\{_iid_}\ProxyStubClsid32`. The value given here is a CLSID, so you should chase that down as well.</dd>

<dt>TYPE_E_CANTLOADLIBRARY (0x80029C4A)</dt>

<dd>When attempting to use typelib-based marshalling, COM couldn't load the .TLB file. Check in `HKEY_CLASSES_ROOT\Typelib\{_typelib-id_}\x.y\0\win32` to verify that the path to the .TLB file is correct. In particular, note that relative paths in this key are a bad idea.</dd>

</dl>
