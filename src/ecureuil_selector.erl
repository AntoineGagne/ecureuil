-module(ecureuil_selector).

%% API
-export([parse/1,
         find/2]).

-export_type([selector/0]).

-type match() :: {match, selector(), selector()}.
-type is_child() :: {is_child, selector(), selector()}.
-type is_sibling() :: {is_sibling, selector(), selector()}.
-type is_adjacent_sibling() :: {is_adjacent_sibling, selector(), selector()}.
-type is_descendant() :: {is_descendant, selector(), selector()}.
-type is_not() :: {is_not, tag(), {'not', selector()}} | {is_not, {'not', selector()}}.
-type nth_child() :: {nth_child, non_neg_integer()}.
-type has_class() :: {has_class, tag(), class()}.
-type has_id() :: {has_id, tag(), id()}.
-type tag() :: {identifer, binary()}.
-type id() :: {id, binary()}.
-type class() :: {class, binary()}.
-opaque selector() :: (
          match()
          | is_child()
          | is_sibling()
          | is_adjacent_sibling()
          | is_descendant()
          | is_not()
          | nth_child()
          | has_class()
          | has_id()
          | tag()
          | id()
          | class()).

%%%===================================================================
%%% API
%%%===================================================================

-spec parse(string() | binary()) -> {ok, selector()} | {error, term()}.
parse(Raw) ->
    Trimmed = string:trim(Raw, both),
    try_parse(Trimmed).

find(Html, Selector) ->
    try_find(Selector, Html).

%%%===================================================================
%%% Internal functions
%%%===================================================================

try_parse(Raw) when is_binary(Raw) ->
    List = unicode:characters_to_list(Raw),
    try_parse(List);
try_parse(Raw) ->
    case ecureuil_lexer:string(Raw) of
        {ok, Tokens, _} ->
            ecureuil_parser:parse(Tokens);
        Error ->
            Error
    end.

try_find(Selector, Html) ->
    {ok, []}.
