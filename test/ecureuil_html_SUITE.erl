-module(ecureuil_html_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(SOME_INVALID_HTML, <<"some invalid</">>).
-define(SOME_VALID_HTML,
        <<"<a class=\"a b\" id=\"c\"><!- test !-><div class=\"a\" id=\"e\">Test</div></a>">>).

all() ->
    [
     return_error_on_invalid_html,
     can_parse_valid_html
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
