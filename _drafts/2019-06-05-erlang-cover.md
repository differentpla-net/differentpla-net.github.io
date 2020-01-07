---
title: Erlang Coverage Trick
date: 2019-06-05 10:32
---

{% raw %}
```
-module(hello).
-export([world/0, kitty/0]).

world() ->
    greet("World").

kitty() ->
    greet("Kitty").

greet(Who) ->
    io:format("Hello ~s~n", [Who]).
```

```
$ erl
1> c(hello).
{ok,hello}
2> hello:world().
Hello World
ok
3> cover:start().
{ok,<0.91.0>}
4> cover:compile(hello).
{ok,hello}
5> hello:world().
Hello World
ok
6> cover:is_compiled(hello).
{file,"/home/roger/Source/imp/imp_server/hello.erl"}
7> cover:analyze().
{result,[{{hello,greet,1},{1,0}},
         {{hello,kitty,0},{0,1}},
         {{hello,world,0},{1,0}}],
        []}
8> cover:analyze_to_file([hello], [html]).
{result,["hello.COVER.html"],[]}
```
{% endraw %}
