---
title: "Helpers for fold accumulators, part 2"
date: 2024-02-13T15:08:00Z
tags: erlang
---

A follow up to [On whether "fold accumulator helpers" are a good idea]({% post_url 2024/2024-02-09-erlang-fold-accumulator-helpers %}).

The previous post discussed whether it was a good idea to introduce helper functions to manage the accumulator in a fold
expression. At the end of that post, I suggested that treating the accumulator as state might be a good idea.

That ended up looking like this:

```erlang
handle_responses(Responses) ->
    lists:foldl(fun handle_response/2, new_acc(), Responses).

handle_response(Response = #{topic := Topic, partitions := Partitions}, Acc) ->
    lists:foldl(fun handle_partition/2, set_topic(Topic, Acc), Partitions).

handle_partition(Partition = #{partition_index := PartitionIndex, records := Records}, Acc) ->
    add_records(PartitionIndex, Records, Acc).
```

```erlang
set_topic(Topic, State) ->
    State#{topic => Topic}.

add_records(PartitionIndex, Records, State = #{topic := Topic, records := Records0}) ->
    State#{records := [{Topic, PartitionIndex, Records} | Records0]}.
```

On balance, I'm not sure that's a great idea -- in my case, at least -- because:

1. There are a few places where I want to store multiple things in the "state", which looks ugly: `...,
   set_base_offset(BaseOffset, set_base_timestamp(BaseTimestamp, Acc)), ...`. This would be improved by a pipeline
   operator, but probably not by much.
2. It's not entirely clear, as you're looking at the nested folds, which values are present in the "state". Did I fill
   in `base_offset`? Is `partition_index` valid at this scope?

## Partial application

It's a pity that Erlang doesn't have syntax for partial function application, but we can do it explicitly. Does this
look better?

```erlang
handle_responses(Responses) ->
    lists:foldl(handle_response(), new_acc(), Responses).

handle_response() ->
    fun(Response = #{topic := Topic, partitions := Partitions}, Acc) ->
        lists:foldl(handle_partition(Topic), Acc, Partitions)
    end.

handle_partition(Topic) ->
    fun(Partition = #{partition_index := PartitionIndex, records := Records}, Acc) ->
        add_records(Topic, PartitionIndex, Records, Acc)
    end.
```

```erlang
set_topic(Topic, State) ->
    State#{topic => Topic}.

add_records(Topic, PartitionIndex, Records, Acc = #{records := Records0}) ->
    Acc#{records := [{Topic, PartitionIndex, Records} | Records0]}.
```

Honestly, I'm not sure. I might just un-nest all of the functions and live with it. It might work in your case, though.