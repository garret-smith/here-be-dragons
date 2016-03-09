-module(print_ast).

-export([parse_transform/2]).

parse_transform(Forms, Options) ->
    io:fwrite("AST(~p):~n~p~n", [Options, Forms]),
    Forms.

