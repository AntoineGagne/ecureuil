-module(ecureuil_selector).

%% API
-export([parse/1,
         find/2
        ]).

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

-spec find(selector(), ecureuil_html:html()) -> {ok, [ecureuil_html:html_node()]} | {error, term()}.
find(Selector, Html) ->
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
    case build_matcher(Selector) of
        Error={error, _} -> Error;
        Matcher ->
            Indices = Matcher(Html),
            ExtractNodes = fun (Index) ->
                                   {ok, Node} = ecureuil_html:index(Index, Html),
                                   Node
                           end,
            {ok, lists:map(ExtractNodes, Indices)}
    end.

build_matcher(all) ->
    fun all/1;
build_matcher({identifier, Identifier}) ->
    identifier(Identifier);
build_matcher({id, Id}) ->
    id(Id);
build_matcher({class, Class}) ->
    class(Class);
build_matcher({has_id, Identifier, Id}) ->
    IdentifierMatcher = build_matcher(Identifier),
    Matcher = build_matcher(Id),
    combine_and(IdentifierMatcher, Matcher);
build_matcher({has_class, Identifier, Class}) ->
    IdentifierMatcher = build_matcher(Identifier),
    Matcher = build_matcher(Class),
    combine_and(IdentifierMatcher, Matcher);
build_matcher({match, M1, M2}) ->
    Matcher1 = build_matcher(M1),
    Matcher2 = build_matcher(M2),
    combine_or(Matcher1, Matcher2);
build_matcher(Invalid={is_child, _Parent, _Child}) ->
    {error, {not_implemented, Invalid}};
build_matcher(Invalid={is_sibling, _S1, _S2}) ->
    {error, {not_implemented, Invalid}};
build_matcher(Invalid={is_adjacent_sibling, _S1, _S2}) ->
    {error, {not_implemented, Invalid}};
build_matcher(Invalid={is_descendant, _Ancestor, _Descendant}) ->
    {error, {not_implemented, Invalid}};
build_matcher({is_not, Identifier, Clause}) ->
    IdentifierMatcher = build_matcher(Identifier),
    Matcher = build_matcher(Clause),
    combine_not(IdentifierMatcher, Matcher);
build_matcher({is_not, Clause}) ->
    Matcher = build_matcher(Clause),
    combine_not(fun all/1, Matcher);
build_matcher(Invalid={nth_child, _Amount}) ->
    {error, {not_implemented, Invalid}};
build_matcher(Invalid) ->
    {error, {unknown, Invalid}}.

combine_not(Matcher1, Matcher2) ->
    fun (Index) ->
            Matched1 = sets:from_list(Matcher1(Index)),
            Matched2 = sets:from_list(Matcher2(Index)),
            Combined = sets:subtract(Matched1, Matched2),
            sets:to_list(Combined)
    end.

combine_and(Matcher1, Matcher2) ->
    fun (Index) ->
            Matched1 = sets:from_list(Matcher1(Index)),
            Matched2 = sets:from_list(Matcher2(Index)),
            Combined = sets:intersection(Matched1, Matched2),
            sets:to_list(Combined)
    end.

combine_or(Matcher1, Matcher2) ->
    fun (Index) ->
            Matched1 = sets:from_list(Matcher1(Index)),
            Matched2 = sets:from_list(Matcher2(Index)),
            Combined = sets:union(Matched1, Matched2),
            sets:to_list(Combined)
    end.

all(Index) ->
    ecureuil_html:all(Index).

id(Id) ->
    fun (Index) -> ecureuil_html:id(Id, Index) end.

class(Class) ->
    fun (Index) -> ecureuil_html:class(Class, Index) end.

identifier(Identifier) ->
    fun (Index) -> ecureuil_html:identifier(Identifier, Index) end.
