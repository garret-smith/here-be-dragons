-module(module).

-compile([{parse_transform, my_transform}]).

-compile(export_all).

-export([hello/1]).

hello(Who) when is_atom(Who) ->
    io:fwrite("hello ~p", [Who]).

