%% @author Antoine Gagné <gagnantoine@gmail.com>
%% @copyright 2019 Antoine Gagné
%% @doc Parse and navigate HTML with CSS selectors.
-module(ecureuil).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([find/2]).

-export_type([]).

-type html_node() :: ecureuil_html:html_node().

%%%===================================================================
%%% API
%%%===================================================================

-spec find(binary() | string(), binary() | string()) -> {ok, [html_node()]} | {error, term()}.
%% @doc Navigate the HTML with the selector and returns all the matching elements.
find(RawSelector, RawHtml) ->
    MaybeHtml = ecureuil_html:parse(RawHtml),
    MaybeSelector = ecureuil_selector:parse(RawSelector),
    maybe_find(MaybeSelector, MaybeHtml).

%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_find(E1={error, _}, E2={error, _}) ->
    {error, {{parse_selector, E1}, {parse_html, E2}}};
maybe_find(E={error, _}, _) ->
    {error, {parse_selector, E}};
maybe_find(_, E={error, _}) ->
    {error, {parse_html, E}};
maybe_find({ok, Selector}, {ok, Html}) ->
    ecureuil_selector:find(Selector, Html).

%%%===================================================================
%%% EUnit Tests
%%%===================================================================

-ifdef(TEST).
-endif.
