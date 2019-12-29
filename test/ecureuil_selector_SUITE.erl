-module(ecureuil_selector_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(AN_INVALID_SELECTOR, <<"<><>invalid<><>">>).
-define(A_VALID_SELECTOR, <<"div.someclass, a.blueurl, a#some-specific-link">>).
-define(SOME_VALID_HTML, <<"<div class=\"someclass\">Valid</div>">>).

all() ->
    [
     return_error_on_invalid_selector,
     can_parse_valid_selector,
     can_find_selected_elements
    ].

init_per_testcase(_Name, Config) ->
    Config.

end_per_testcase(_Name, Config) ->
    Config.

return_error_on_invalid_selector() ->
    [{doc, "Given an invalid selector, when parsing, then returns an error."}].
return_error_on_invalid_selector(_Config) ->
    ?assertMatch({error, _}, ecureuil_selector:parse(?AN_INVALID_SELECTOR)).

can_parse_valid_selector() ->
    [{doc, "Given a valid selector, when parsing, then returns parsed selector."}].
can_parse_valid_selector(_Config) ->
    ?assertMatch({ok, _}, ecureuil_selector:parse(?A_VALID_SELECTOR)).

can_find_selected_elements() ->
    [{doc, "Given a valid selector, when finding elements, then returns elements found."}].
can_find_selected_elements(_Config) ->
    {ok, Html} = ecureuil_html:parse(?SOME_VALID_HTML),
    {ok, Selector} = ecureuil_selector:parse(?A_VALID_SELECTOR),

    ?assertMatch({ok, [{<<"div">>, _, _}]}, ecureuil_selector:find(Selector, Html)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
