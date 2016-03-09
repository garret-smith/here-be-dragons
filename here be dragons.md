class: center, bottom, inverse

# HERE BE DRAGONS
![Map](img/Map.png)
## charting parse transforms in Erlang
---
## Our journey
.left-column[
  ### What is a parse transform?
]

.right-column[
  ## Erlang Manual:

  .boxed[
    Programmers are strongly advised not to engage in parse transforms
    and no support is offered for problems encountered.
  ]
]
---
## Our journey
.left-column[
  ### What is a parse transform?
  ### Program as data
]

.right-column[
  ## The Abstract Format
]
---
## Our journey
.left-column[
  ### What is a parse transform?
  ### Program as data
  ### Working with Abstract Format
]

.right-column[
  ## Bring your sword to the dragon fight
]
---
.left-column[
  ## Our journey
  ### What is a parse transform?
  ### Program as data
  ### Working with Abstract Format
  ### Parse transforms in the wild
]

.right-column[
  ## Other monsters in the menagerie
]
---
.left-column[
  ## Our journey
  ### What is a parse transform?
  ### Program as data
  ### Working with Abstract Format
  ### Parse transforms in the wild
  ### Inline parse transform
]

.right-column[
  ## Have the dragon eat it's own tail
]
---
class: middle, center, inverse
# What is a parse transform?
---
class: middle

.pull-left[
Source

&darr;

Compiler

&darr;

Executable

]

.pull-right[
`module.erl`

&darr;

```sh
> erlc module.erl
```

&darr;

`module.beam`
]

---
# Classic compilation steps
.pull-left[
1. Expand macros
2. Lex
3. Parse
4. Abstract syntax tree
5. Optimize
6. Generate bytecode
]
.pull-right[
![Aho](img/aho.jpg)
]

---
# Classic compilation steps
1. Expand macros
2. Lex
3. Parse
4. Abstract syntax tree
5. .highlighted[&rarr;] &nbsp; Transformed syntax tree = `your_function(AST)` &nbsp; .highlighted[&larr;]
6. Optimize
7. Generate bytecode

---
# Identity transform

```erlang
-module(my_transform).

-export([parse_transform/2]).

parse_transform(Forms, Options) ->
  Forms.
```

---
# Invoke a parse transform
erlc
```sh
erlc my_transform.erl
erlc -pa . +"{parse_transform,my_transform}" test.erl
```

rebar.config
```erlang
{erl_opts, [
            {parse_transform, my_transform}
           ]}.
```

module inline
```erlang
-module(test).

-compile([{parse_transform, my_transform}]).

...
```
---
class: center, middle, inverse

# Abstract Format
## Your program as data
---
# Abstract Syntax Tree

.pull-left[
### A + B * C
![graphs/a_bc.png](graphs/a_bc.png)
]
--
.pull-right[
### (A + B) * C
![graphs/ab_c.png](graphs/ab_c.png)
]
---
# Abstract Syntax Tree

.pull-left[
### A + B * C
![graphs/a_bc.png](graphs/a_bc.png)
```erlang
{'+',
  A,
  {'*', B, C}}
```
]
.pull-right[
### (A + B) * C
![graphs/ab_c.png](graphs/ab_c.png)
```erlang
{'*',
  {'+', A, B},
  C}
```
]
---
```erlang
-module(test).

-compile([{parse_transform, my_transform}]).

-export([hello/1]).

hello(Who) ->
    io:fwrite("hello ~p", [Who]).
```

```erlang
[{attribute,1,file,{"test.erl",1}},
 {attribute,1,module,test},
 {attribute,3,compile,[]},
 {attribute,5,export,[{hello,1}]},
 {function,7,hello,1,
           [{clause,7,
                    [{var,7,'Who'}],
                    [],
                    [{call,8,
                           {remote,8,{atom,8,io},{atom,8,fwrite}},
                           [{string,8,"hello ~p"},
                            {cons,8,{var,8,'Who'},{nil,8}}]}]}]},
 {eof,10}]
```
---
```erlang
*-module(test).

-compile([{parse_transform, my_transform}]).

-export([hello/1]).

hello(Who) ->
    io:fwrite("hello ~p", [Who]).
```

```erlang
[{attribute,1,file,{"test.erl",1}},
* {attribute,1,module,test},
 {attribute,3,compile,[]},
 {attribute,5,export,[{hello,1}]},
 {function,7,hello,1,
           [{clause,7,
                    [{var,7,'Who'}],
                    [],
                    [{call,8,
                           {remote,8,{atom,8,io},{atom,8,fwrite}},
                           [{string,8,"hello ~p"},
                            {cons,8,{var,8,'Who'},{nil,8}}]}]}]},
 {eof,10}]
```
---
```erlang
-module(test).

*-compile([{parse_transform, my_transform}]).

-export([hello/1]).

hello(Who) ->
    io:fwrite("hello ~p", [Who]).
```

```erlang
[{attribute,1,file,{"test.erl",1}},
 {attribute,1,module,test},
* {attribute,3,compile,[]},
 {attribute,5,export,[{hello,1}]},
 {function,7,hello,1,
           [{clause,7,
                    [{var,7,'Who'}],
                    [],
                    [{call,8,
                           {remote,8,{atom,8,io},{atom,8,fwrite}},
                           [{string,8,"hello ~p"},
                            {cons,8,{var,8,'Who'},{nil,8}}]}]}]},
 {eof,10}]
```
---
```erlang
-module(test).

-compile([{parse_transform, my_transform}]).

*-export([hello/1]).

hello(Who) ->
    io:fwrite("hello ~p", [Who]).
```

```erlang
[{attribute,1,file,{"test.erl",1}},
 {attribute,1,module,test},
 {attribute,3,compile,[]},
* {attribute,5,export,[{hello,1}]},
 {function,7,hello,1,
           [{clause,7,
                    [{var,7,'Who'}],
                    [],
                    [{call,8,
                           {remote,8,{atom,8,io},{atom,8,fwrite}},
                           [{string,8,"hello ~p"},
                            {cons,8,{var,8,'Who'},{nil,8}}]}]}]},
 {eof,10}]
```
---
```erlang
-module(test).

-compile([{parse_transform, my_transform}]).

-export([hello/1]).

*hello(Who) ->
    io:fwrite("hello ~p", [Who]).
```

```erlang
[{attribute,1,file,{"test.erl",1}},
 {attribute,1,module,test},
 {attribute,3,compile,[]},
 {attribute,5,export,[{hello,1}]},
* {function,7,hello,1,
*           [{clause,7,
*                    [{var,7,'Who'}],
*                    [],
                    [{call,8,
                           {remote,8,{atom,8,io},{atom,8,fwrite}},
                           [{string,8,"hello ~p"},
                            {cons,8,{var,8,'Who'},{nil,8}}]}]}]},
 {eof,10}]
```
---
```erlang
-module(test).

-compile([{parse_transform, my_transform}]).

-export([hello/1]).

* `hello`(`Who`) ->
    io:fwrite("hello ~p", [Who]).
```

```erlang
[{attribute,1,file,{"test.erl",1}},
 {attribute,1,module,test},
 {attribute,3,compile,[]},
 {attribute,5,export,[{hello,1}]},
* {function,7,`hello`,1,
*           [{clause,7,
*                    [{var,7,'`Who`'}],
*                    [],
                    [{call,8,
                           {remote,8,{atom,8,io},{atom,8,fwrite}},
                           [{string,8,"hello ~p"},
                            {cons,8,{var,8,'Who'},{nil,8}}]}]}]},
 {eof,10}]
```
---
```erlang
-module(test).

-compile([{parse_transform, my_transform}]).

-export([hello/1]).

*hello(Who)` `->
    io:fwrite("hello ~p", [Who]).
```

```erlang
[{attribute,1,file,{"test.erl",1}},
 {attribute,1,module,test},
 {attribute,3,compile,[]},
 {attribute,5,export,[{hello,1}]},
 {function,7,hello,1,
           [{clause,7,
                    [{var,7,'Who'}],
*                   `[]`,
                    [{call,8,
                           {remote,8,{atom,8,io},{atom,8,fwrite}},
                           [{string,8,"hello ~p"},
                            {cons,8,{var,8,'Who'},{nil,8}}]}]}]},
 {eof,10}]
```
---
```erlang
-module(test).

-compile([{parse_transform, my_transform}]).

-export([hello/1]).

hello(Who) ->
*   io:fwrite("hello ~p", [Who]).
```

```erlang
[{attribute,1,file,{"test.erl",1}},
 {attribute,1,module,test},
 {attribute,3,compile,[]},
 {attribute,5,export,[{hello,1}]},
 {function,7,hello,1,
           [{clause,7,
                    [{var,7,'Who'}],
                    [],
*                   [{call,8,
*                          {remote,8,{atom,8,io},{atom,8,fwrite}},
*                          [{string,8,"hello ~p"},
*                           {cons,8,{var,8,'Who'},{nil,8}}]}]}]},
 {eof,10}]
```
---
```erlang
-module(test).

-compile([{parse_transform, my_transform}]).

-export([hello/1]).

hello(Who) ->
*   `io`:`fwrite`("hello ~p", [Who]).
```

```erlang
[{attribute,1,file,{"test.erl",1}},
 {attribute,1,module,test},
 {attribute,3,compile,[]},
 {attribute,5,export,[{hello,1}]},
 {function,7,hello,1,
           [{clause,7,
                    [{var,7,'Who'}],
                    [],
*                   [{call,8,
*                          {remote,8,{atom,8,`io`},{atom,8,`fwrite`}},
*                          [{string,8,"hello ~p"},
*                           {cons,8,{var,8,'Who'},{nil,8}}]}]}]},
 {eof,10}]
```
---
```erlang
-module(test).

-compile([{parse_transform, my_transform}]).

-export([hello/1]).

hello(Who) ->
    io:fwrite(`"hello ~p", [Who]`).
```

```erlang
[{attribute,1,file,{"test.erl",1}},
 {attribute,1,module,test},
 {attribute,3,compile,[]},
 {attribute,5,export,[{hello,1}]},
 {function,7,hello,1,
           [{clause,7,
                    [{var,7,'Who'}],
                    [],
                    [{call,8,
                           {remote,8,{atom,8,io},{atom,8,fwrite}},
                           `[{string,8,"hello ~p"},`
                            `{cons,8,{var,8,'Who'},{nil,8}}]`}]}]},
 {eof,10}]
```
---
class: middle, center, inverse
# Setting Sail
## Transform some parse trees!
---
# Hypothetical application

Whenever the ETS table `contentious_table` is accessed, fire off a message to a
tracker process with the ETS method used: select / insert / update / delete.

The tracker process could then accumulate number of calls / time.  You could
begin to get an idea of what processes are using this table and how often.

--
## Problem

- Find: `ets:insert(contentious_table, Objects)`

--
- Insert: `ets_collector ! {insert, self()}`

--
- Bonus: `ets_collector ! {insert, length(Objects), self()}`

---
### What does that look like in AF???

```erlang
1> {ok, Tokens, _} =
       erl_scan:string("ets:insert(contentious_table, Objects).").

2> {ok, Forms} = erl_parse:parse_exprs(Tokens).

3> Forms.
[{call,1,
       {remote,1,{atom,1,ets},{atom,1,insert}},
       [{atom,1,contentious_table},{var,1,'Objects'}]}]
```

--
OK, match on
```erlang
{call, _,
 {remote, _, {atom, _, `ets`}, {atom, _, `insert`}},
 [{atom, _, `contentious_table`}, {var, _, `'Objects'`}]}
```

???
Just make sure not to match on `Objects` since the variable name might be
different.

---
### ... and insert ...

```erlang
1> {ok, Tokens, _} =
       erl_scan:string("ets_collector ! {insert, self()}.").

2> {ok, Forms} = erl_parse:parse_exprs(Tokens).

3> Forms.
[{op,1,'!',
     {atom,1,ets_collector},
     {tuple,1,[{atom,1,insert},{call,1,{atom,1,self},[]}]}}]
```
---
# Transform function!
```erlang
transform_ets_insert({call, Line,
                       {remote, _, {atom, _, ets},
                         {atom, _, insert}},
                       [{atom, _, contentious_table},
                         {var, _, Objects}]}
                     = Form) ->
    {op,Line,'!',
      {atom,Line,ets_collector},
      {tuple,Line,[{atom,Line,insert},
                   {call,Line,{atom,Line,self},[]}]}};
transform_ets_insert(Form) ->
    Form.

```
--
... well, replace at least ...
---
# Transform this
```erlang
change_username(UserId, UserName) when UserId =:= 0 ->
  error("Can't change admin user name");
change_username(UserId, UserName) ->
  [User] = ets:lookup(contentious_table, UserId),
  case User#user.group of
    luser -> error("Insufficient privileges");
    _ -> `ets:insert(contentious_table`, User#user{name = UserName})
  end.
```
---
.smaller[
```erlang
{function,13,change_username,2,
    [{clause,13,
        [{var,13,'UserId'},{var,13,'UserName'}],
        [[{op,13,'=:=',{var,13,'UserId'},{integer,13,0}}]],
        [{call,14,
              {atom,14,error},
              [{string,14,"Can't change admin user name"}]}]},
    {clause,15,
        [{var,15,'UserId'},{var,15,'UserName'}],
        [],
        [{match,16,
              {cons,16,{var,16,'User'},{nil,16}},
              {call,16,
                  {remote,16,{atom,16,ets},{atom,16,lookup}},
                  [{atom,16,contentious_table},{var,16,'UserId'}]}},
          {case,17,
              {record_field,17,{var,17,'User'},user,{atom,17,group}},
              [{clause,18,
                  [{atom,18,luser}],
                  [],
                  [{call,18,
                        {atom,18,error},
                        [{string,18, "Insufficient privileges"}]}]},
              {clause,19,
                  [{var,19,'_'}],
                  [],
                  [{call,19,
                        {remote,19,{atom,19,`ets`},{atom,19,`insert`}},
                        [{atom,19,`contentious_table`},
                        {record,19,
                            {var,19,'User'},
                            user,
                            [{record_field,19,
                                  {atom,19,name},
                                  {var,19,'UserName'}}]}]}]}]}]}]}
```
]
---
class: middle, center, inverse
# Lets take a step back

# &nbsp;

![img/run-away.png](img/run-away.png)
---
# Isn't there something to help?

## `stdlib`
- epp - Erlang preprocessor (macros and includes)
- erl_eval - Execute ASTs
- erl_id_trans - An identity transform that walks the whole AST
- erl_parse - Create the AST from tokens
- erl_pp - Turn an AST back into source code
- erl_scan - Create the tokens from text

## `syntax_tools`
- erl_prettypr - Another pretty-printer of ASTs
- erl_syntax - Define a 'super-set' of the `stdlib` AST
- erl_syntax_lib - Helper functions for working with ASTs
---
# Common pattern

1. Find some AST node, ie a function call

2. Extract context / detail

3. Modify the node / insert a new one

4. Update the AST

5. Return the new AST to the compiler
---
## erl_syntax_lib
```erlang
map(Fun, Tree) -> NewTree.

fold(Fun, Acc, Tree) -> NewAcc.
```
---
class: middle, center, inverse
# Working with Abstract Format
## Bring your sword to the dragon fight
![img/sword-destiny.png](img/sword-destiny.png)
---
# parse_trans

[https://github.com/uwiger/parse_trans.git](https://github.com/uwiger/parse_trans.git)

Many convenience functions for working with parse_transforms.
---
class: middle, center, inverse
# Why _not_ parse transforms?
---
class: middle, center
## Your bug just became a compiler bug

## Potential to create difficult-to-reason-about code

## Slow down the compiler
---
class: center, middle, inverse
# Parse transforms in the wild
---
# aeon*
## Another Erlang to Object Notation translator

[https://github.com/garret-smith/aeon](https://github.com/garret-smith/aeon)

Extract `-type()` information from a module, including `-record` definitions,
and make it available at runtime.  Use this type information to drive
conversion of JSON back and forth to Erlang records or `-type`s.

.footnote[*I wrote it]
---
# exprecs
## part of the parse_trans project

[https://github.com/uwiger/parse_trans.git](https://github.com/uwiger/parse_trans.git)

Generate and export accessor functions for record fields.
---
# lager (as in beer)

[https://github.com/basho/lager.git](https://github.com/basho/lager.git)

Logging!  The parse transform turns calls like `lager:info("message")` into
`lager:dispatch_log(Where, info, {Line, }, "message", [], ...)` along with even
more checking before making the call to see if the log level is even handled.
---
# Thanks

[Steal this map](http://www.ruleofthedice.com/2011/02/steal-this-map.html)
[exprecs]
