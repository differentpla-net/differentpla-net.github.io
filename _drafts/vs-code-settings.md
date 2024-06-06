From scratch.

Enable settings sync, so I never have to do this again.

Colour/Icon scheme

Themes -> Color Scheme -> Dark+
Themes -> File Icon Theme -> Install vscode-icons; enable it

If I install an extension in the base profile, is it available in other profiles?

Create a new Erlang profile. Do I want Work Erlang / Personal Erlang profiles? Probably do: work requires Kafka, home doesn't.

Profiles -> Create Profile -> Name: Erlang, Icon: the icon selection is pretty poor.

If you don't copy from the default, it's a completely blank slate.

Note that you can use a different theme for each profile, which might help. You'll also need to reinstall your preferred icon sets.

For Erlang:
- Dark+
- vscode-icons
- TODO:
  - Turn off editor preview
  - Turn off breadcrumbs
  - Extensions:
    - ShellCheck
    - erlang-ls
    - because I have rebar3 and erlfmt managed by direnv, I'm gonna need mkhl.direnv
    - because I don't like the formatter in erlang-ls, gonna need szTheory.erlang-formatter, and it needs configuring to use erlfmt.
    - usernamehw.errorlens
    - aaron-bond.better-comments
    - stkb.rewrap; set rewrap to 120
    - Maybe: peacock

So: put some of the above in the default profile and copy from that when
creating a new profile. I don't think that changes to the source profile are
propogated again, though.

Bugs:

something's screwing up the indent-on-newline, similar to my home laptop.

