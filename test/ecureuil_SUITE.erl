-module(ecureuil_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(SOME_VALID_HTML, <<"<a href=\"http://localhost:443\"">>).
-define(A_VALID_SELECTOR, <<"a">>).
-define(SOME_INVALID_HTML, <<">>">>).
-define(AN_INVALID_SELECTOR, <<"<>invalid<>">>).

all() ->
    [
     return_find_error_on_invalid_html,
     return_find_error_on_invalid_selector,
     return_find_error_on_invalid_html_and_selector,
     can_find_element
    ].

init_per_testcase(_Name, Config) ->
    meck:new(ecureuil_html),
    meck:new(ecureuil_selector),

    meck:expect(ecureuil_html, parse, [{[?SOME_VALID_HTML], {ok, #{}}},
                                       {[?SOME_INVALID_HTML], {error, error}}]),
    meck:expect(ecureuil_selector, parse, [{[?A_VALID_SELECTOR], {ok, selector}},
                                           {[?AN_INVALID_SELECTOR], {error, error}}]),
    meck:expect(ecureuil_selector, find, fun (_, _) -> {ok, []} end),
    Config.

end_per_testcase(_Name, Config) ->
    meck:unload(),
    Config.

return_find_error_on_invalid_html() ->
    [{doc, "Given some invalid HTML, when finding, then returns an error."}].
return_find_error_on_invalid_html(_Config) ->
    ?assertMatch({error, _}, ecureuil:find(?A_VALID_SELECTOR, ?SOME_INVALID_HTML)).

return_find_error_on_invalid_selector() ->
    [{doc, "Given an invalid selector, when finding, then returns an error."}].
return_find_error_on_invalid_selector(_Config) ->
    ?assertMatch({error, _}, ecureuil:find(?AN_INVALID_SELECTOR, ?SOME_VALID_HTML)).

return_find_error_on_invalid_html_and_selector() ->
    [{doc, "Given some invalid HTML and selector, when finding, then return an error."}].
return_find_error_on_invalid_html_and_selector(_Config) ->
    ?assertMatch({error, _}, ecureuil:find(?AN_INVALID_SELECTOR, ?SOME_INVALID_HTML)).

can_find_element() ->
    [{doc, "Given some valid HTML and a valid selector, when finding, "
      "then returns found elements."}].
can_find_element(_Config) ->
    ?assertMatch({ok, _}, ecureuil:find(?A_VALID_SELECTOR, ?SOME_VALID_HTML)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
