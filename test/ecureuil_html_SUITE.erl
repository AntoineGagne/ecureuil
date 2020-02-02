-module(ecureuil_html_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(SOME_INVALID_HTML, <<"some invalid</">>).
-define(AN_ATTRIBUTE, <<"an attribute">>).
-define(SOME_VALID_HTML,
        <<"<a class=\"a b\" id=\"c\"><!- test !-><div class=\"a\" id=\"e\">Test</div></a>">>).
-define(AN_EXISTING_ID, "c").
-define(AN_EXISTING_CLASS, "a").
-define(AN_EXISTING_IDENTIFIER, "a").
-define(A_NON_EXISTING_NODE, 1000).
-define(AN_EXISTING_NODE, 0).
-define(A_COMMENT, {comment, <<" comment --!></div>">>}).

all() ->
    [
     return_error_on_invalid_html,
     can_parse_valid_html,
     can_fetch_nodes_with_matching_id,
     can_fetch_nodes_with_matching_classes,
     can_fetch_nodes_with_matching_identifiers,
     return_error_on_unknown_node,
     can_fetch_node
    ].

init_per_testcase(_Name, Config) ->
    meck:new(mochiweb_html, [passthrough]),
    Config.

end_per_testcase(_Name, Config) ->
    meck:unload(),
    Config.

return_error_on_invalid_html() ->
    [{doc, "Given invalid HTML, when parsing, then returns an error."}].
return_error_on_invalid_html(_Config) ->
    ?assertMatch({error, _}, ecureuil_html:parse(?SOME_INVALID_HTML)).

can_parse_valid_html() ->
    [{doc, "Given valid HTML, when parsing, then returns parsed HTML."}].
can_parse_valid_html(_Config) ->
    ?assertMatch({ok, _}, ecureuil_html:parse(?SOME_VALID_HTML)).

can_fetch_nodes_with_matching_id() ->
    [{doc, "Given valid HTML, when fetching existing ID, then returns corresponding nodes."}].
can_fetch_nodes_with_matching_id(_Config) ->
    {ok, Index} = ecureuil_html:parse(?SOME_VALID_HTML),
    ?assertMatch([0], ecureuil_html:id(?AN_EXISTING_ID, Index)).

can_fetch_nodes_with_matching_classes() ->
    [{doc, "Given valid HTML, when fetching existing class, then returns corresponding nodes."}].
can_fetch_nodes_with_matching_classes(_Config) ->
    {ok, Index} = ecureuil_html:parse(?SOME_VALID_HTML),
    Classes = ecureuil_html:class(?AN_EXISTING_CLASS, Index),
    ?assertMatch([0, 2], lists:sort(Classes)).

can_fetch_nodes_with_matching_identifiers() ->
    [{doc, "Given valid HTML, when fetching existing identifier, then returns corresponding "
      "nodes."}].
can_fetch_nodes_with_matching_identifiers(_Config) ->
    {ok, Index} = ecureuil_html:parse(?SOME_VALID_HTML),
    Identifiers = ecureuil_html:identifier(?AN_EXISTING_IDENTIFIER, Index),
    ?assertMatch([0], Identifiers).

return_error_on_unknown_node() ->
    [{doc, "Given valid HTML, when fetching non-existing node, then returns an error."}].
return_error_on_unknown_node(_Config) ->
    {ok, Index} = ecureuil_html:parse(?SOME_VALID_HTML),
    ?assertMatch({error, _}, ecureuil_html:index(?A_NON_EXISTING_NODE, Index)).

can_fetch_node() ->
    [{doc, "Given valid HTML, when fetching existing node, then returns corresponding node."}].
can_fetch_node(_Config) ->
    {ok, Index} = ecureuil_html:parse(?SOME_VALID_HTML),
    ?assertMatch({ok, {_, _, _}}, ecureuil_html:index(?AN_EXISTING_NODE, Index)).

return_error_when_fetching_attribute_from_comment() ->
    [{doc, "Given an HTML comment, when fetching attribute, then returns an error."}].
return_error_when_fetching_attribute_from_comment(_Config) ->
    ?assertMatch({error, _}, ecureuil_html:attribute(?A_COMMENT, ?AN_ATTRIBUTE)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
