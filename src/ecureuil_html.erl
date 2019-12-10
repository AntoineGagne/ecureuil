-module(ecureuil_html).

%% API
-export([parse/1,
         attribute/2]).

-export_type([html/0,
              html_node/0,
              attribute/0]).

-type attribute() :: {binary(), binary()}.
-type html_node() :: {binary(), [attribute()], [html_node() | binary()]}.
-opaque html() :: html_node().

%%%===================================================================
%%% API
%%%===================================================================

-spec parse(binary() | string()) -> {ok, html()} | {error, term()}.
parse(Raw) ->
    try
        {ok, mochiweb_html:parse(Raw)}
    catch
        E:R -> {error, {E, R}}
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
