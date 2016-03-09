-module(my_transform).

-export([
         parse_transform/2,
         print_type/1
        ]).

parse_transform(Forms, _Options) ->
    [erl_syntax_lib:map(fun print_type/1, F) || F <- Forms],
    % io:fwrite("~p~n", [Forms]),
    Forms.

print_type(F) ->
    io:fwrite("~p : ~p~n", [erl_syntax:type(F), F]),
    F.

