
-module(ets_transform).

-export([
         parse_transform/2,
         transform_ets_insert/1
        ]).

parse_transform(Forms, _Options) ->
    Forms1 = [erl_syntax_lib:map(
                fun(Node) ->
                    transform_ets_insert(
                      erl_syntax:revert(Node))
                end,
                F)
              || F <- Forms],
    erl_syntax:revert_forms(Forms1).

transform_ets_insert({call, Line,
                       {remote, _, {atom, _, ets}, {atom, _, insert}},
                       [{atom, _, contentious_table}, _Objects]}
                     = Form) ->
    {block, Line, [{op,Line,'!',
                    {atom,Line,ets_collector},
                    {tuple,Line,[{atom,Line,insert},
                                {call,Line,{atom,Line,self},[]}]}},
                   Form]};
transform_ets_insert(Form) ->
    Form.

