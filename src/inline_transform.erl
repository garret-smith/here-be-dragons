
-module(inline_transform).

% A parse transform that enables inline parse transforms.
% Inline parse transforms are defined by an 'inline_transform' directive

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
        AF = erl_syntax_lib:analyze_forms(Forms),

        TransformFunction = find_inline_transform(attributes(AF)),
        Transformer = find_transformer_forms(Forms, TransformFunction),
        io:fwrite("~p~n", [Transformer]),
        Forms.

attributes(AnalyzedForms) ->
        {attributes, A} = lists:keyfind(attributes, 1, AnalyzedForms),
        A.

find_inline_transform(Attributes) ->
        hd([F || {inline_transform, F} <- Attributes]).

find_transformer_forms([Form | Forms], TransformFunctionName) ->
        case erl_syntax:type(Form) of
                function ->
                        {Name, Arity} = erl_syntax_lib:analyze_function(Form),
                        case Name == TransformFunctionName of
                                true -> Form;
                                false -> find_transformer_forms(Forms, TransformFunctionName)
                        end;
                _ -> find_transformer_forms(Forms, TransformFunctionName)
        end.

