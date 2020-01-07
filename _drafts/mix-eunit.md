# mix eunit with an Erlang umbrella project

```
mix new drunk_turing --umbrella
cd drunk_turing
git start
```

`git start` is an alias:

```
[alias]
    start = !git init \
        && git commit --allow-empty -m \"Initial commit\" \
        && git tag -am \"Initial commit\" initial-commit
```

```
git add .
mix
```

```
cd apps
mix new stoic_morse
git add stoic_morse
```

```
cd ../..
mix
```

To install various plugins, we use (e.g.):

```
mix archive.install hex phx_new
```

What do we do if we want project-local mix plugins?

## Resources

- https://frightanic.com/goodies_content/docker-names.php
- https://elixirschool.com/en/lessons/basics/mix-tasks/

