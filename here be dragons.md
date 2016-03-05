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
  ### Erlang Manual
  Programmers are strongly advised not to engage in parse transforms
  and no support is offered for problems encountered.
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

.float-left[
Source

&darr;

Compiler

&darr;

Executable

]

.float-right[
```
module.erl
```

&darr;

```sh
> erlc module.erl
```

&darr;

```
module.beam
```
]

---
# Classic compilation steps
.float-left[
1. Expand macros
2. Lex
3. Parse
4. Abstract syntax tree
5. Optimize
6. Generate bytecode
]
.float-right[
![Aho](img/aho.jpg)
]
---
# Classic compilation steps
1. Expand macros
2. Lex
3. Parse
4. .highlighted[ Abstract syntax tree ]
5. Optimize
6. Generate bytecode
---
# Abstract Syntax

## Identity transform

```erlang
parse_transform(AbstractSyntaxTree, Options) ->
  AbstractSyntaxTree.
```
---
class: center, middle, inverse

# Abstract Format
## Your program as data
---

---
# Thanks

[Map graphic](http://www.ruleofthedice.com/2011/02/steal-this-map.html)


