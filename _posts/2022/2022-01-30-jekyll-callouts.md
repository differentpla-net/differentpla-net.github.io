---
title: "Jekyll Callouts"
date: 2022-01-30T18:04:00Z
tags: jekyll liquid
---

Sometimes [alerts]({% post_url 2021/2021-12-17-jekyll-alerts %}) are a bit, well, alert-y. Here are some callouts instead.

<div class="callout callout-info" markdown="span">
This is a callout. It can be used to make an aside without disrupting the flow of the main document.
</div>

Instead of using an include (which makes sense because of the icons), these can just be pasted straight in:

```html
<div class="callout callout-info" markdown="span">
This is a callout. It can be used to make an aside without disrupting the flow of the main document.
</div>
```

## Examples

<div class="callout callout-primary" markdown="span">
A primary callout.
</div>

<div class="callout callout-secondary" markdown="span">
A secondary callout.
</div>

<div class="callout callout-success" markdown="span">
A success callout.
</div>

<div class="callout callout-danger" markdown="span">
A danger callout.
</div>

<div class="callout callout-warning" markdown="span">
A warning callout.
</div>

<div class="callout callout-info" markdown="span">
An info callout.
</div>

<div class="callout callout-light" markdown="span">
A light callout.
</div>

<div class="callout callout-dark" markdown="span">
A dark callout.
</div>
