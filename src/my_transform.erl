-module(my_transform).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    io:fwrite("~p~n", [Forms]),
    Forms.

