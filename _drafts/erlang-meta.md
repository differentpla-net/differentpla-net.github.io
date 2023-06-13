Context: attempting to convert Kafka's JSON schema definitions into Erlang code to encode and decode the messages.

Problem: the JSON format is kinda awkward to deal with. Tried to run it through bbmustache (not powerful enough); eel (too new). Both require pre-processing the JSON (or the map loaded from it) before handing to the template engine.

So maybe it's easier to explicitly pre-process the map into an Erlang DSL (either explicit function calls, builder interface; or terms), and let _that_ drive the code generation.

Note, aside: the aws_erlang stuff uses EEx to generate the Erlang/Elixir code. But I think that's simpler to parse.

That takes us to the next step: the idea of generating the code through a template is to make it easier to understand. If we've got to pre-process it anyway, why not just skip that step and generate AST instead? We can probably turn that back into core Erlang, which will be fairly readable anyway.

We _can't_ turn the BEAM back into Core Erlang, though, unless we embed debug information (which is the AST, and a link to the original code, iirc).

So: what does the AST look like for some basic things?


rlipscombe/code-generation [main ?1] % vi foo.erl
rlipscombe/code-generation [main ?1] % erlc +to_pp foo.erl
rlipscombe/code-generation [main ?2] % ls
foo.P  foo.erl
rlipscombe/code-generation [main ?2] % cat foo.P
{attribute,{1,1},file,{"foo.erl",1}}.
{attribute,{1,2},module,foo}.
{attribute,{2,2},export,[{encode,0}]}.
{function,
    {4,1},
    encode,0,
    [{clause,
         {4,1},
         [],[],
         [{match,
              {5,2},
              {var,{5,2},'Body'},
              {bin,
                  {5,9},
                  [{bin_element,{5,11},{integer,{5,11},1},default,default},
                   {bin_element,{5,14},{integer,{5,14},2},default,default}]}},
          {match,
              {6,2},
              {var,{6,2},'Size'},
              {call,{6,9},{atom,{6,9},iolist_size},[{var,{6,21},'Body'}]}},
          {cons,
              {7,2},
              {bin,
                  {7,3},
                  [{bin_element,
                       {7,5},
                       {var,{7,5},'Size'},
                       {integer,{7,10},32},
                       [big]},
                   {bin_element,{7,18},{var,{7,18},'Body'},default,default}]},
              {nil,{7,24}}}]}]}.
{eof,{8,1}}.
rlipscombe/code-generation [main ?2] % erl
Erlang/OTP 24 [erts-12.2.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [jit]

Eshell V12.2.1  (abort with ^G)
1> {ok, P} = file:read_file("foo.P").
{ok,<<"{attribute,{1,1},file,{\"foo.erl\",1}}.\n{attribute,{1,2},module,foo}.\n{attribute,{2,2},export,[{encode,0}]}.\n{"...>>}
2> compile:f
file/1          file/2          format_error/1  forms/1
forms/2
2> compile:forms(P).
: internal error in pass transform_module:
exception error: no function clause matching compile:compile_options(<<"{attribute,{1,1},file,{\"foo.erl\",1}}.\n{attribute,{1,2},module,foo}.\n{attribute,{2,2},export,[{encode,0}]}.\n{function,\n    {4,1},\n    encode,0,\n    [{clause,\n         {4,1},\n         [],[],\n         [{match,\n              {5,2},\n              {var,{5,2},'Body'},\n              {bin,\n                  {5,9},\n                  [{bin_element,{5,11},{integer,{5,11},1},default,default},\n                   {bin_element,{5,14},{integer,{5,14},2},default,default}]}},\n          {match,\n              {6,2},\n              {var,{6,2},'Size'},\n              {call,{6,9},{atom,{6,9},iolist_size},[{var,{6,21},'Body'}]}},\n          {cons,\n              {7,2},\n              {bin,\n                  {7,3},\n                  [{bin_element,\n                       {7,5},\n                       {var,{7,5},'Size'},\n
       {integer,{7,10},32},\n                       [big]},\n                   {bin_element,{7,18},{var,{7,18},'Body'},default,default}]},\n              {nil,{7,24}}}]}]}.\n{eof,{8,1}}.\n">>) (compile.erl, line 1101)
  in function  compile:transform_module/2 (compile.erl, line 1126)
  in call from compile:fold_comp/4 (compile.erl, line 418)
  in call from compile:internal_comp/5 (compile.erl, line 402)
  in call from compile:'-internal_fun/2-anonymous-0-'/2 (compile.erl, line 229)
  in call from compile:'-do_compile/2-anonymous-0-'/1 (compile.erl, line 219)
error
3> compile:forms(P, []).
error
4> compile:forms(P).
: internal error in pass transform_module:
exception error: no function clause matching compile:compile_options(<<"{attribute,{1,1},file,{\"foo.erl\",1}}.\n{attribute,{1,2},module,foo}.\n{attribute,{2,2},export,[{encode,0}]}.\n{function,\n    {4,1},\n    encode,0,\n    [{clause,\n         {4,1},\n         [],[],\n         [{match,\n              {5,2},\n              {var,{5,2},'Body'},\n              {bin,\n                  {5,9},\n                  [{bin_element,{5,11},{integer,{5,11},1},default,default},\n                   {bin_element,{5,14},{integer,{5,14},2},default,default}]}},\n          {match,\n              {6,2},\n              {var,{6,2},'Size'},\n              {call,{6,9},{atom,{6,9},iolist_size},[{var,{6,21},'Body'}]}},\n          {cons,\n              {7,2},\n              {bin,\n                  {7,3},\n                  [{bin_element,\n                       {7,5},\n                       {var,{7,5},'Size'},\n
       {integer,{7,10},32},\n                       [big]},\n                   {bin_element,{7,18},{var,{7,18},'Body'},default,default}]},\n              {nil,{7,24}}}]}]}.\n{eof,{8,1}}.\n">>) (compile.erl, line 1101)
  in function  compile:transform_module/2 (compile.erl, line 1126)
  in call from compile:fold_comp/4 (compile.erl, line 418)
  in call from compile:internal_comp/5 (compile.erl, line 402)
  in call from compile:'-internal_fun/2-anonymous-0-'/2 (compile.erl, line 229)
  in call from compile:'-do_compile/2-anonymous-0-'/1 (compile.erl, line 219)
error
5> P.
<<"{attribute,{1,1},file,{\"foo.erl\",1}}.\n{attribute,{1,2},module,foo}.\n{attribute,{2,2},export,[{encode,0}]}.\n{function"...>>
6> file:consult("foo.P").
{ok,[{attribute,{1,1},file,{"foo.erl",1}},
     {attribute,{1,2},module,foo},
     {attribute,{2,2},export,[{encode,0}]},
     {function,{4,1},
               encode,0,
               [{clause,{4,1},
                        [],[],
                        [{match,{5,2},
                                {var,{5,2},'Body'},
                                {bin,{5,9},
                                     [{bin_element,{5,...},{...},...},{bin_element,{...},...}]}},
                         {match,{6,2},
                                {var,{6,2},'Size'},
                                {call,{6,9},{atom,{6,...},iolist_size},[{var,...}]}},
                         {cons,{7,2},
                               {bin,{7,3},[{bin_element,{...},...},{bin_element,...}]},
                               {nil,{7,24}}}]}]},
     {eof,{8,1}}]}
7> {ok, Forms} = file:consult("foo.P").
{ok,[{attribute,{1,1},file,{"foo.erl",1}},
     {attribute,{1,2},module,foo},
     {attribute,{2,2},export,[{encode,0}]},
     {function,{4,1},
               encode,0,
               [{clause,{4,1},
                        [],[],
                        [{match,{5,2},
                                {var,{5,2},'Body'},
                                {bin,{5,9},
                                     [{bin_element,{5,...},{...},...},{bin_element,{...},...}]}},
                         {match,{6,2},
                                {var,{6,2},'Size'},
                                {call,{6,9},{atom,{6,...},iolist_size},[{var,...}]}},
                         {cons,{7,2},
                               {bin,{7,3},[{bin_element,{...},...},{bin_element,...}]},
                               {nil,{7,24}}}]}]},
     {eof,{8,1}}]}
8> compile:forms(Forms).
{ok,foo,
    <<70,79,82,49,0,0,2,20,66,69,65,77,65,116,85,56,0,0,0,62,
      0,0,0,6,3,102,...>>}
9> {ok, _, Beam} = compile:forms(Forms).
{ok,foo,
    <<70,79,82,49,0,0,2,20,66,69,65,77,65,116,85,56,0,0,0,62,
      0,0,0,6,3,102,...>>}
10> code:load_
load_abs/1             load_abs/2             load_binary/3
load_file/1            load_native_partial/2  load_native_sticky/3

10> code:get_object_code(Beam).
** exception error: no function clause matching code:get_object_code(<<70,79,82,49,0,0,2,20,66,69,65,77,65,116,85,56,0,0,0,
                                                                       62,0,0,0,6,3,102,111,111,...>>) (code.erl, line 251)
11> code:get_object_code(foo).
error
12> code:load_
load_abs/1             load_abs/2             load_binary/3
load_file/1            load_native_partial/2  load_native_sticky/3

12> code:load_binary(foo, undefined, Beam).
{module,foo}
13> foo:
encode/0       module_info/0  module_info/1
13> foo:
encode/0       module_info/0  module_info/1
13> foo:encode().
** exception error: bad argument
     in function  foo:encode/0 (foo.erl, line 7)
14>

Still seeing a lot of actual variable names in the forms, because of the match stuff, etc.; wonder if there's a way to ellide that, or whether that's actually a bad idea anyway?

Might be worth looking at EEl to see how it generates the forms anyway? Could be some useful tricks in there.

Later thoughts: it's all super-ugly, and will result in really ugly generation code.

I think the best option might be to pre-process the JSON into DSL/terms and then process that into Erlang source code. Maybe, not least, because that's a half-way step, and allows it to be integrated into the existing stuff.
