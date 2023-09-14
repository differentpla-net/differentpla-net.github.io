---
title: "How I make presentations"
date: 2023-09-14T13:22:00.000Z
---

I dislike Powerpoint and I can't get on with Visio. Here's how I make my presentations.

## Marp

I like Markdown, so I use [Marp](https://marp.app/) to make my slides. To use it, write some Markdown as normal, and add the following Frontmatter:

```yaml
---
marp: true
theme: gaia
_class: gaia
---
```

I prefer the "Gaia" theme over the default. You can tweak the styles in Markdown, or make your own theme from scratch.
See the Marp docs for details.

To preview the Markdown in Visual Studio Code, you can add the `marp-team.marp-vscode` extension, then use the "Preview Markdown" command. To switch between slides and not-slides, change the `marp: true` line in the Frontmatter.

To convert your markdown to HTML, you'll need to install the Marp CLI. If you're using Homebrew, that's `brew install
marp-cli`.

## D2

For architecture diagrams, [D2](https://www.d2lang.com/) does a reasonable job. The CLI can be installed with `brew
install d2`, and the VS Code extension ID is `Terrastruct.d2`.

To embed your diagram in your slides, you'll need to convert the `.d2` file to a `.svg` file with the following:

```sh
d2 architecture.d2 architecture.svg
```

Then you can embed the image in your slide deck. Use Marp's `bg` directive to tweak the size and location:

```md
![bg contain](architecture.svg)
```

## Mermaid

For sequence diagrams, I like [Mermaid](https://mermaid.js.org/). It can also do some of the things that D2 can do, so
feel free to mix and match them.

Assuming you've got NodeJS and NPM, you can install it like this:

```sh
npm install -g @mermaid-js/mermaid-cli
```

To convert your `.mmd` file to a `.svg`:

```
mmdc -i sequence.mmd -o sequence.svg
```

Then embed it in your slide deck, as above:

```md
![bg 50%](sequence.svg)
```

## Makefile

The Makefile I use for all of this is in [this gist](https://gist.github.com/rlipscombe/293a2d8d217deb5cdb73bcb2e5d75ecd).
