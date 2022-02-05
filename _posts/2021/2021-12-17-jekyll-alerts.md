---
title: "Jekyll Alerts"
date: 2021-12-17T09:25:00Z
tags: jekyll liquid
---

I wanted to display an [alert](https://getbootstrap.com/docs/5.1/components/alerts/) on one of my pages, like this:

{% include alerts/danger.html content="**Here be dragons**<br/>I'm going to let you into a secret..." %}

I found [a page](https://idratherbewriting.com/documentation-theme-jekyll/mydoc_alerts.html) that explained how to use Liquid's {% raw %}`{% include %}`{% endraw %} to do it. It uses Liquid's includes, [passing parameters](https://jekyllrb.com/docs/includes/#passing-parameters-to-includes).

Based on that, here's what I came up with.

```liquid
{% raw %}{% include alerts/danger.html content="**Here be dragons**<br/>I'm going to let you into a secret..." %}{% endraw %}
```

The `_includes/alerts/danger.html` file looks like this:

```html
{% raw %}<div class="alert alert-danger d-flex" role="alert">
    <i class="bi bi-exclamation-triangle-fill"></i>
    <div markdown="span" style="margin-left: 0.7em;">
        {{ include.content | strip }}
    </div>
</div>{% endraw %}
```

I've got corresponding examples ([see below](#examples)) for the other Bootstrap alert types.

## Using Markdown

To allow markdown in alerts, the content `div` has `markdown="span"`. Note that you can only use markdown on spans, not blocks, so you're unable to use (e.g.) `<p>...</p>` in the alert, and will have to make do with `<br/>`.

## Nesting Liquid

If you want to use Liquid (e.g. `{% raw %}{% post_url %}{% endraw %}`) in your alerts, you need to capture the HTML first, like this:

```liquid
{% raw %}{% capture warning %}
**Warning:** An alternative rail-replacement bus service is in operation this weekend.<br/>
For updated timetables, click [here]({% post_url 2021/2021-12-17-jekyll-alerts %}).
{% endcapture %}
{% include alerts/warning.html content=warning %}{% endraw %}
```

{% capture warning %}
**Warning:** An alternative rail replacement bus service is in operation this weekend.<br/>
For updated timetables, click [here]({% post_url 2021/2021-12-17-jekyll-alerts %}).
{% endcapture %}
{% include alerts/warning.html content=warning %}

## Icons

The icons are from the [Octicons](https://github.com/primer/octicons). With the `jekyll-octicons` plugin, they can be used like this:

```liquid
{% raw %}{% octicon alert height:24 %}{% endraw %}
```

I tried to allow overriding the icon, but couldn't get it to work.

## Examples

{% include alerts/primary.html content="A primary alert" %}
{% include alerts/secondary.html content="A secondary alert" %}
{% include alerts/success.html content="A success alert" %}
{% include alerts/danger.html content="A danger alert" %}
{% include alerts/warning.html content="A warning alert" %}
{% include alerts/info.html content="An info alert" %}
{% include alerts/light.html content="A light alert" %}
{% include alerts/dark.html content="A dark alert" %}
