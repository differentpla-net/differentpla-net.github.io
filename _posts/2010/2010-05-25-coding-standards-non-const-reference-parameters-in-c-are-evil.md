---
title: "Coding Standards: Non-const reference parameters in C++ are evil"
date: 2010-05-25T14:08:46.000Z
x-drupal-nid: 253
x-needs-review: 2010-05-25T14:08:46.000Z
---
This is just a minor rant:

In C++, const references are useful for parameters, because they avoid copying the arguments to a method. Non-const references, on the other hand, are pure evil, because there’s no way (at the call site) to immediately see that a method might change a variable:

<div id="codeSnippetWrapper">
<div id="codeSnippet" class="csharpcode">
<pre class="alt"><span class="kwrd">int</span> expectedVersion = 1;</pre>

<pre class="alteven">ValidateVersion(expectedVersion);</pre>

</div>

</div>

By looking at this, you can’t tell that ValidateVersion is declared as void `ValidateVersion(int &version)` and might actually change the value.

Use a pointer instead:

<div id="codeSnippetWrapper">
<div id="codeSnippet" class="csharpcode">
<pre class="alt">ValidateVersion(&expectedVersion);</pre>

</div>

</div>

Then, when I’m looking at the call site, I have an expectation that the value might change.
