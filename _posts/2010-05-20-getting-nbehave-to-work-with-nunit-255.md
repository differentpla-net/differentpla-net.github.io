---
title: "Getting NBehave to work with NUnit 2.5.5"
date: 2010-05-20T14:37:51.000Z
x-drupal-nid: 251
x-needs-review: 2010-05-20T14:37:51.000Z
---
NBehave depends on NUnit 2.5.2\. To get it to work with NUnit 2.5.5, you can create an NBehave-Console.exe.config file containing the following:

<span style="color: blue"><?</span><span style="color: #a31515">xml </span><span style="color: red">version</span><span style="color: blue">=</span>"<span style="color: blue">1.0</span>" <span style="color: red">encoding</span><span style="color: blue">=</span>"<span style="color: blue">utf-8</span>" <span style="color: blue">?>   
<</span><span style="color: #a31515">configuration</span><span style="color: blue">>   
</span><span style="color: blue">  <</span><span style="color: #a31515">runtime</span><span style="color: blue">>   
    <</span><span style="color: #a31515">assemblyBinding </span><span style="color: red">xmlns</span><span style="color: blue">=</span>"<span style="color: blue">urn:schemas-microsoft-com:asm.v1</span>"<span style="color: blue">>   
      <</span><span style="color: #a31515">dependentAssembly</span><span style="color: blue">>   
        <</span><span style="color: #a31515">assemblyIdentity </span><span style="color: red">name</span><span style="color: blue">=</span>"<span style="color: blue">nunit.framework</span>"   
                          <span style="color: red">culture</span><span style="color: blue">=</span>"<span style="color: blue">neutral</span>"   
                          <span style="color: red">publicKeyToken</span><span style="color: blue">=</span>"<span style="color: blue">96d09a1eb7f44a77</span>" <span style="color: blue">/>   
        <</span><span style="color: #a31515">bindingRedirect </span><span style="color: red">oldVersion</span><span style="color: blue">=</span>"<span style="color: blue">2.5.2.9222</span>" <span style="color: red">newVersion</span><span style="color: blue">=</span>"<span style="color: blue">2.5.5.10112</span>" <span style="color: blue">/>   
      </</span><span style="color: #a31515">dependentAssembly</span><span style="color: blue">>   
    </</span><span style="color: #a31515">assemblyBinding</span><span style="color: blue">>   
  </</span><span style="color: #a31515">runtime</span><span style="color: blue">>   
</</span><span style="color: #a31515">configuration</span><span style="color: blue">>  
</span>

This will cause it to use 2.5.5.10112 instead of 2.5.2.9222\. You’ll also need to overwrite the copy of nunit.framework in the NBehave directory with the newer version.