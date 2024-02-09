---
title: "Helpers for fold accumulators"
date: 2024-02-09T10:41:00Z
tags: erlang
---

On whether "fold accumulator helpers" are a good idea.

I'm currently writing some code that recursively folds over a multi-level data structure, and I found myself wondering
whether introducing helper functions specifically to abstract over the accumulator are a good idea.

To recap, in Erlang, `lists:foldl/3` takes an accumulator:

```erlang
% Calculate the product of the items in the list.
lists:foldl(
    fun(Item, Acc) ->
        Item * Acc
    end, 1, List).
```

## Problem

In the code I'm writing at the moment (something Kafka-related), I'm having to recursively fold over a nested data
structure, and I need to keep track of several things at once in the accumulator.

It looks something like the following:

```erlang
lists:foldl(
    fun(Response = #{topic := Topic, partitions := Partitions}, Acc1) ->
        lists:foldl(
            fun(Partition = #{partition_index := PartitionIndex, error_code := ?NONE, records := Records}, Acc2) ->
                % Simplified. Add 'Records', preserve 'Errors'.
                {Records0, Errors} = Acc2,
                {[{Topic, PartitionIndex, Records} | Records0], Errors};
               (Partition = #{partition_index := PartitionIndex, error_code := Error}, Acc2) ->
                % Simplified. Preserve 'Records', add 'Error'.
                {Records, Errors0} = Acc2,
                {Records, [{Topic, PartitionIndex, Error} | Errors0]}
            end, Acc1, Partitions)
    end, {[], []}, Responses).
```

The actual nested structure is 4-deep, rather than 2-deep.

The idea is that, after all of the processing, we end up with two lists: one of records and one of errors. But, even
though this is a simplified snippet, the "shape" of the accumulator, which is not pretty, has been scattered in multiple
places in the code.

If we want to change the shape -- maybe we need to add another element to the tuple, or maybe we decide we want to use a
map -- we need to change it in all of those places. This is a bad thing: it makes us less likely to make sweeping
changes, for example. It only gets harder when you realise that Erlang doesn't really have static types.

## Proposal

So, instead, I'm wondering whether introducing helper functions, specifically to abstract over the accumulator, would be
a good idea. It would look something like this:

```erlang
lists:foldl(
    fun(Response = #{topic := Topic, partitions := Partitions}, Acc1) ->
        lists:foldl(
            fun(Partition = #{partition_index := PartitionIndex, error_code := ?NONE, records := Records}, Acc2) ->
                add_records(Topic, PartitionIndex, Records, Acc2);
               (Partition = #{partition_index := PartitionIndex, error_code := Error}, Acc2) ->
                add_error(Topic, PartitionIndex, Error, Acc2)
            end, Acc1, Partitions)
    end, new_acc(), Responses).
```

The helpers would look like this:

```erlang
new_acc() ->
    {[], []}.

add_records(Topic, PartitionIndex, Records, Acc = {Records0, Errors}) ->
    {[{Topic, PartitionIndex, Records} | Records0], Errors}.

add_error(Topic, PartitionIndex, Error, Acc = {Records, Errors0}) ->
    {Records, [{Topic, PartitionIndex, Error} | Errors]}.
```

This would allow us to make changes to the accumulator more easily. For example, we could decide to flatten the records:

```erlang
add_records(Topic, PartitionIndex, Records, Acc = {Records0, Errors}) ->
    Records1 = Records0 ++ [{Topic, PartitionIndex, Record} || Record <- Records],
    {Records1, Errors}.
```

## Accumulator as State

A related idea is to treat the accumulator as state:

```erlang
lists:foldl(
    fun(Response = #{topic := Topic, partitions := Partitions}, Acc1) ->
        lists:foldl(
            fun(Partition = #{partition_index := PartitionIndex, error_code := ?NONE, records := Records}, Acc2) ->
                add_records(PartitionIndex, Records, Acc2);
               (Partition = #{partition_index := PartitionIndex, error_code := Error}, Acc2) ->
                add_error(PartitionIndex, Error, Acc2)
            end, set_topic(Topic, Acc1), Partitions)
    end, new_acc(), Responses).
```

```erlang
set_topic(Topic, State) ->
    State#{topic => Topic}.

add_records(PartitionIndex, Records, State = #{topic := Topic, records := Records0}) ->
    State#{records := [{Topic, PartitionIndex, Records} | Records0]}.
```

I'm not sure about the above, because 'Topic' is already available from the outer scope, but it might become more useful
if we wanted to reduce nesting:

```erlang
handle_responses(Responses) ->
    lists:foldl(fun handle_response/2, new_acc(), Responses).

handle_response(Response = #{topic := Topic, partitions := Partitions}, Acc) ->
    lists:foldl(fun handle_partition/2, set_topic(Topic, Acc), Partitions).

% and so on.
```
