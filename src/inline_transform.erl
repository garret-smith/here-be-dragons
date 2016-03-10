
-module(inline_transform).

% A parse transform that enables inline parse transforms.
% Inline parse transforms are defined by an 'inline_transform' directive

-export([parse_transform/2]).

parse_transform(Forms, Options) ->
    {[InlineTransform], RemainingForms} =
        lists:partition(
          fun({function, _, inline_transform, 2, _}) -> true;
             (_) -> false
          end,
          Forms),
    TransformerExpressions = extract_exprs(InlineTransform),

    {value, Transformed, _Vars} =
        erl_eval:exprs(TransformerExpressions,
                       orddict:from_list([{'Forms', RemainingForms},
                                          {'Options', Options}])),
    erl_syntax:revert_forms(Transformed).

extract_exprs({function, _Line, _Name, _Arity, Clauses}) ->
    {clause, _, _Args, _When, Exprs} = hd(Clauses),
    Exprs.

