---
title: "Improving your quality of life as a developer"
date: 2023-10-27T12:53:00.000Z
---

I've just come off a call with a colleague and while he was sharing his screen and we were walking through some things,
I noticed that his setup is missing all of the little "quality of life" tweaks that I've made over the years.

## Tab Completion

As Scott Hanselman says:

> There are a finite number of keystrokes left in your hands before you die.

He's talking about [a different topic](https://www.hanselman.com/blog/do-they-deserve-the-gift-of-your-keystrokes), but
it still applies here. Every time you have to type out the full name of a file, that's wasted keystrokes which you could
be using for something better.

So: **enable tab completion** in your shell of choice. Enable it for _everything_. I'm currently using Zsh with [Oh My
Zsh](https://ohmyz.sh/), and I've got tab completion enabled for `git`, for `kubectl`, for `docker` and for many other
commands that I use daily or hourly.

## History search

You knew that you can press the Up key to recall previous commands already. But did you know that you can press Ctrl+R
to _search_ those previous commands?

You want to run `docker compose up`? Well, you could type it in from scratch, _again_, or you could press `Up` half a
dozen times, or you could press `Ctrl+R`, type `up` and then press `Enter`. Much quicker. Fewer typos.

A related tip for this is Ned Batcheler's [bashtags](https://nedbatchelder.com/blog/201307/hashtags_for_commands.html).

## Prettify your prompt

You're not just doing this because it looks cool. You're doing it because you're typing `git status` fifty times a day
because you forgot which branch you were working on. **Get a nicer prompt**.

As mentioned, I'm using Oh My Zsh, which provides themes. Check them out. Edit one of those to suit your preferences.
Write your own. Other shells provide similar functionality.

You could even check out [Starship](https://starship.rs/) which provides a customizable cross-shell prompt. I didn't get
on with it, but it's _your_ prompt. Do whatever you want with it.

## Shell Aliases

I'm not a huge fan of shell aliases (I lean on tab completion), but another one of my colleagues loves them.

He uses them for jumping to different directories (need to get to the CI pipeline for the "qa" environment? just type
`qa`).

The various Oh My Zsh plugins add extra ones (need a list of Kubernetes pods? Just type `kgpo` -- or something; I
disabled the aliases).

## Git Aliases

What I _do_ have, is several useful aliases for Git. For example, `git st` gives me a shortened version of `git status`
which displays _just_ the information I need to know. `git ci` is `git commit`, but it also displays the diff right
there in the editor, so that I can jog my memory about what I changed.

## Learn your $EDITOR properly

Learn how to use your editor properly. Install a bunch plugins to improve your quality of life. If you're using vim,
learn some of the basic keystrokes. You'll save so much more time, and you'll look like a genius to your mouse-using
colleagues.
