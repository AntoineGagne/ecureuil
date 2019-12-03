Header "%% Copyright (C)"
       "%% @private"
       "%% @author Antoine Gagné".

Nonterminals
selectors.

Terminals
id class '>' identifier.

Rootsymbol selectors.

selectors -> selectors '>' selectors : {is_child, '$1', '$3'}.
selectors -> identifier id : {has_id, extract_token('$1'), extract_token('$2')}.
selectors -> identifier class : {has_class, extract_token('$1'), extract_token('$2')}.
selectors -> class : {has_class, extract_token('$1')}.
selectors -> id : {has_id, extract_token('$1')}.

Erlang code.

extract_token({_Token, _Line, Value}) ->
    list_to_binary(Value).
