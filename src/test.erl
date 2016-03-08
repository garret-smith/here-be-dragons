-module(test).

-compile([{parse_transform, my_transform}]).

-export([hello/1]).

hello(Who) when is_atom(Who) ->
    io:fwrite("hello ~p", [Who]).

count_ys(Terms) ->
    lists:foldl(fun(T, A) ->
                        case T of
                            x -> A;
                            y -> A+1
                        end
                end,
                0,
                Terms).

