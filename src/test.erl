-module(test).

-compile([{parse_transform, my_transform}]).

-export([hello/1]).

mapfold_xs(F, Terms) ->
    lists:foldl(fun({process_me, X}, A) ->
                        [F(X) | A];
                   (_, A) ->
                        A
                end,
                [],
                Terms).

hello(Who) when is_atom(Who) ->
    io:fwrite("hello ~p", [Who]).

