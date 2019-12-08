Header "%% Copyright (C)"
       "%% @private"
       "%% @author Antoine Gagné".

Nonterminals
selector.

Terminals
id all class identifier close_parentheses pseudo_not pseudo pseudo_class_integer
'>' '+' '~' ' ' ','.

Rootsymbol selector.

selector -> selector ',' selector : {match, '$1', '$3'}.
selector -> selector '>' selector : {is_child, '$1', '$3'}.
selector -> selector '~' selector : {is_sibling, '$1', '$3'}.
selector -> selector '+' selector : {is_adjacent_sibling, '$1', '$3'}.
selector -> selector ' ' selector : {is_descendant, '$1', '$3'}.
selector -> identifier pseudo_not selector close_parentheses : {is_not, to_binary('$1'), '$3'}.
selector -> pseudo_not selector close_parentheses : {is_not, '$2'}.
selector -> pseudo pseudo_class_integer : {to_atom('$1'), extract_token('$2')}.

selector -> all : all.
selector -> identifier : {identifier, to_binary('$1')}.
selector -> identifier id : {has_id, to_binary('$1'), to_binary('$2')}.
selector -> identifier class : {has_class, to_binary('$1'), to_binary('$2')}.
selector -> class : {has_class, to_binary('$1')}.
selector -> id : {has_id, to_binary('$1')}.

Erlang code.

extract_token({_Token, _Line, Value}) ->
    Value.

to_binary({_Token, _Line, Value}) ->
    list_to_binary(Value).

to_atom({_Token, _Line, "nth-child"}) ->
    nth_child.
