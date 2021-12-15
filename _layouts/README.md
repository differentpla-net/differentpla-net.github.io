# A note about layouts

Because when I came to update the theme, I realised I'd forgotten how this works.

Some pages specify their layout explicitly. These are:

- `/index.html`, which uses `layout: home`
- `/404.html`, which uses `layout: default`
- The various blog posts which are in a series. They use `layout: series`,
  which adds a sidebar nav for the other pages in the series.

Posts and drafts are configured `_config.yml`. They use `layout: post`.

The layouts are related as follows:

- `_layouts/home.html` is self-contained. It includes a few things and then just renders the content.
- `_layouts/default.html` is, well, the default. It's not meaningfully different from `home`.
- `_layouts/post.html` is the default for posts. It derives from `default`, and just renders an `<article>`.
- `_layouts/series.html` is used for series-posts. It also derives from `default`. It renders an `<article>`, and the series-sidebar.
