-module(ecureuil_parser).

%% API
-export([parse/1]).

%%%===================================================================
%%% API
%%%===================================================================

parse(Raw) ->
    try_parse(Raw).

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
