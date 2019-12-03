-module(ecureuil_parser).

%% API
-export([]).

%%%===================================================================
%%% API
%%%===================================================================

parse(Raw) ->
    case try_parse(Raw) of
        {ok, Tree} -> {ok, flatten(Tree)};
        Error -> Error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

try_parse(Raw) ->
    case ecureuil_lexer:string(Raw) of
        {ok, Tokens, _} ->
            ecureuil_generated_parser:parse(Tokens);
        Error ->
            Error
    end.

flatten(Value) ->
    Value.
