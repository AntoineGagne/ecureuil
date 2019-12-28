-module(ecureuil_html).

%% API
-export([parse/1,
         tree/1,
         attribute/2]).

-export_type([html/0,
              html_node/0,
              attribute/0]).

-type attribute() :: {binary(), binary()}.
-type html_node() :: {binary(), [attribute()], [html_node() | binary()]}.
-opaque html() :: #{tree := html_node(),
                    by_ids := #{},
                    by_identifiers := #{}}.

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
        E:R -> {error, {E, R}}
    end.

-spec tree(html()) -> html_node().
tree(#{tree := Tree}) ->
    Tree.

-spec attribute(html_node(), string() | binary()) -> {ok, binary()} | {error, term()}.
attribute(Node, Attribute) when is_list(Attribute) ->
    Binary = unicode:characters_to_binary(Attribute),
    attribute(Node, Binary);
attribute({_, Attributes, _}, Attribute) ->
    case lists:keyfind(Attribute, 1, Attributes) of
        false -> {error, {not_found, Attribute}};
        {_, Matched} -> {ok, Matched}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

build_index(Parsed) ->
    ByIds = build_tree_by_indices(Parsed),
    ByIdentifiers = build_tree_by_identifiers(ByIds),
    #{tree => Parsed, by_ids => ByIds, by_identifiers => ByIdentifiers}.
    %{_, ByIds} = build_tree_by_indices2({0, []}, Parsed),
    %#{tree => Parsed, by_ids => maps:from_list(ByIds)}.

build_tree_by_indices(Parsed) ->
    {_, ByIds} = do_build_tree_by_indices2({0, []}, Parsed),
    ByIds.

do_build_tree_by_indices2({Start, Index}, {Identifier, Attributes, Children}) ->
    Build = fun (Node, {I, Acc}) ->
                    do_build_tree_by_indices2({I, Acc}, Node)
            end,
    {Start2, Index2} = lists:foldl(Build, {Start + 1, []}, Children),
    ChildrenIndices = lists:map(fun ({I, _}) -> I end, Index2),
    Index3 = [{Start, {Identifier, Attributes, ChildrenIndices}} | Index],
    {Start2, Index3 ++ Index2};
do_build_tree_by_indices2({Start, Index}, Leaf) ->
    {Start + 1, [{Start, Leaf} | Index]}.

% do_build_tree_by_indices({Start, Index}, {Identifier, Attributes, Children}) ->
%     Build = fun (Node, {I, Acc}) ->
%                     do_build_tree_by_indices({I, Acc}, Node)
%             end,
%     {Start2, Index2} = lists:foldl(Build, {Start + 1, Index}, Children),
%     Index3 = Index2#{Start => {Identifier, Attributes, lists:seq(Start + 1, Start2 - 1)}},
%     {Start2, Index3};
% do_build_tree_by_indices({Start, Index}, Leaf) ->
%     {Start + 1, Index#{Start => Leaf}}.

build_tree_by_identifiers(ByIds) ->
    U = fun (K, {Identifier, _, _}, A) ->
                Update = fun (Keys) -> [K | Keys] end,
                maps:update_with(Identifier, Update, [K], A);
            (_, _, A) ->
                A
        end,
    maps:fold(U, #{}, ByIds).
