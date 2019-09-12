---
title: Using Bouncy Castle from .NET
date: 2013-03-18T18:50:00Z
tags: bouncy-castle
layout: series
series: bouncy-castle
---

<ul>
{% for post in site.posts reversed %}
{% if post.series == page.series %}
{% if post.url == page.url %}
<li><a href="{{ post.url }}">{{ post.title }}</a> (this post)</li>
{% else %}
<li><a href="{{ post.url }}">{{ post.title }}</a></li>
{% endif %}
{% endif %}
{% endfor %}
</ul>
