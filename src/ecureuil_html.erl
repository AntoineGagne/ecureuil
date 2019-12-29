-module(ecureuil_html).

%% API
-export([parse/1,
         index/2,
         id/2,
         class/2,
         identifier/2,
         attribute/2]).

-export_type([html/0,
              html_node/0,
              attribute/0]).

-type attribute() :: {binary(), binary()}.
-type html_node() :: {binary(), [attribute()], [index()]}.
-type id() :: binary().
-type class() :: binary().
-type index() :: non_neg_integer().
-type html_identifier() :: binary().
-type by_ids() :: #{id() := [index()]}.
-type by_classes() :: #{class() := [index()]}.
-type by_indices() :: #{index() := html_node()}.
-type by_identifiers() :: #{html_identifier() := [index()]}.

-type not_found(Value) :: {error, {not_found, Value}}.

-opaque html() :: #{by_ids := by_ids(),
                    by_classes := by_classes(),
                    by_indices := by_indices(),
                    by_identifiers := by_identifiers()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec parse(binary() | string()) -> {ok, html()} | {error, term()}.
parse(Raw) ->
    try
        Parsed = mochiweb_html:parse(Raw),
        Indices = build_index(Parsed),
        {ok, Indices}
    catch
        E:R:S -> {error, {E, R, S}}
    end.

-spec attribute(html_node(), string() | binary()) -> {ok, binary()} | {error, term()}.
attribute(Node, Attribute) when is_list(Attribute) ->
    Binary = unicode:characters_to_binary(Attribute),
    attribute(Node, Binary);
attribute({_, Attributes, _}, Attribute) ->
    case lists:keyfind(Attribute, 1, Attributes) of
        false -> {error, {not_found, Attribute}};
        {_, Matched} -> {ok, Matched}
    end.

-spec index(index(), html()) -> {ok, html_node()} | not_found(index()).
index(Index, #{by_indices := ByIndices}) ->
    case maps:find(Index, ByIndices) of
        {ok, Node} -> {ok, Node};
        error -> {error, {not_found, Index}}
    end.

-spec class(string() | binary(), html()) -> [index()].
class(Raw, Index) when is_list(Raw) ->
    Class = unicode:characters_to_binary(Raw),
    class(Class, Index);
class(Class, #{by_classes := ByClasses}) ->
    maps:get(Class, ByClasses, []).

-spec id(string() | binary(), html()) -> [index()].
id(Raw, Index) when is_list(Raw) ->
    Id = unicode:characters_to_binary(Raw),
    id(Id, Index);
id(Id, #{by_ids := ByIds}) ->
    maps:get(Id, ByIds, []).

-spec identifier(string() | binary(), html()) -> [index()].
identifier(Raw, Index) when is_list(Raw) ->
    Identifier = unicode:characters_to_binary(Raw),
    identifier(Identifier, Index);
identifier(Identifier, #{by_identifiers := ByIdentifiers}) ->
    maps:get(Identifier, ByIdentifiers, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

build_index(Parsed) ->
    ByIndices = build_tree_by_indices(Parsed),
    ByIdentifiers = build_tree_by_identifiers(ByIndices),
    ByClasses = build_tree_by_classes(ByIndices),
    ByIds = build_tree_by_ids(ByIndices),
    #{by_indices => ByIndices,
      by_identifiers => ByIdentifiers,
      by_ids => ByIds,
      by_classes => ByClasses}.

build_tree_by_indices(Parsed) ->
    {_, ByIndices} = do_build_tree_by_indices({0, []}, Parsed),
    maps:from_list(ByIndices).

do_build_tree_by_indices({Start, Index}, {Identifier, Attributes, Children}) ->
    Build = fun (Node, {I, Acc}) -> do_build_tree_by_indices({I, Acc}, Node) end,
    {Start2, Index2} = lists:foldl(Build, {Start + 1, []}, Children),
    ChildrenIndices = lists:map(fun ({I, _}) -> I end, Index2),
    Index3 = [{Start, {Identifier, Attributes, ChildrenIndices}} | Index],
    {Start2, Index3 ++ Index2};
do_build_tree_by_indices({Start, Index}, Leaf) ->
    {Start + 1, [{Start, {Leaf, [], []}} | Index]}.

build_tree_by_identifiers(ByIds) ->
    U = fun (K, {Raw, _, _}, A) ->
                Identifier = unicode:characters_to_binary(Raw),
                Update = fun (Keys) -> [K | Keys] end,
                maps:update_with(Identifier, Update, [K], A)
        end,
    maps:fold(U, #{}, ByIds).

build_tree_by_classes(ByIndices) ->
    U = fun (Index, Node, A) ->
                Classes = classes(Node),
                UpdateNodes = fun (Indices) -> [Index | Indices] end,
                Update = fun (Id, Acc) -> maps:update_with(Id, UpdateNodes, [Index], Acc) end,
                lists:foldl(Update, A, Classes)
        end,
    maps:fold(U, #{}, ByIndices).

build_tree_by_ids(ByIndices) ->
    U = fun (Index, Node, A) ->
                Ids = ids(Node),
                UpdateNodes = fun (Indices) -> [Index | Indices] end,
                Update = fun (Id, Acc) -> maps:update_with(Id, UpdateNodes, [Index], Acc) end,
                lists:foldl(Update, A, Ids)
        end,
    maps:fold(U, #{}, ByIndices).

classes(Node) ->
    case attribute(Node, <<"class">>) of
        {error, _} -> [];
        {ok, Raw} ->
            Trimmed = string:trim(Raw),
            Split = string:lexemes(Trimmed, [$\t, $ ]),
            lists:map(fun unicode:characters_to_binary/1, Split)
    end.

ids(Node) ->
    case attribute(Node, <<"id">>) of
        {error, _} -> [];
        {ok, Raw} ->
            Trimmed = string:trim(Raw),
            Split = string:lexemes(Trimmed, [$\t, $ ]),
            lists:map(fun unicode:characters_to_binary/1, Split)
    end.
