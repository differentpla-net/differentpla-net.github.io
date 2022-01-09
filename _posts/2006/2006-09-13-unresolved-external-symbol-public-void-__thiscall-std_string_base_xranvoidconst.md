---
title: "unresolved external symbol \"public: void __thiscall std::_String_base::_Xran(void)const"
date: 2006-09-13T14:20:54.000Z
redirect_from: /content/2006/09/unresolved-external-symbol-std-string-base-xran
---
When compiling a C++ program with Visual C++ 2003, you might get the error:

```
error LNK2019: unresolved external symbol
"public: void __thiscall std::_String_base::_Xran(void)const " (?_Xran@_String_base@std@@QBEXXZ)

referenced in function
"public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > &
__thiscall std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::erase(unsigned int,unsigned int)"
(?erase@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QAEAAV12@II@Z)
```

This is because Microsoft, in their infinite wisdom, have put the VC6 version of libcp.lib in the Platform SDK. And if you've got the Platform SDK\Lib directory before the Vc7\Lib directory in your settings, you'll get this library, which doesn't match the include file in Vc7\Include.

To fix it, either delete this bogus file, or choose a multi-threaded (_MT) or MSVCRT.DLL (_DLL) build.
