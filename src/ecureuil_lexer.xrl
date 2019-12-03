%% The MIT License (MIT)
%%
%% Copyright (c) 2014 Philip Sampaio Silva
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

Definitions.

IDENTIFIER = [-A-Za-z0-9_]+
QUOTED = (\"[^"]*\"|\'[^']*\')
WILDCARD = \*
PARENTESIS = \([^)]*\)
INTEGER = [0-9]+
NOT = (n|N)(o|O)(t|T)
ODD = (o|O)(d|D)(d|D)
EVEN = (e|E)(v|V)(e|E)(n|N)
PSEUDO_PATTERN = (\+|-)?({INTEGER})?(n|N)((\+|-){INTEGER})?
SYMBOL = [\[\]*]
W = [\s\t\r\n\f]

Rules.

{IDENTIFIER}                         : {token, {identifier, TokenLine, TokenChars}}.
{WILDCARD}                           : {token, {all, TokenLine}}.
{QUOTED}                             : {token, {quoted, TokenLine, remove_wrapper(TokenChars)}}.
{SYMBOL}                             : {token, {TokenChars, TokenLine}}.
#{IDENTIFIER}                        : {token, {id, TokenLine, tail(TokenChars)}}.
\.{IDENTIFIER}                       : {token, {class, TokenLine, tail(TokenChars)}}.
\:{NOT}\(                            : {token, {pseudo_not, TokenLine}}.
\:{IDENTIFIER}                       : {token, {pseudo, TokenLine, tail(TokenChars)}}.
\({INTEGER}\)                        : {token, {pseudo_class_integer, TokenLine, list_to_integer(remove_wrapper(TokenChars))}}.
\({ODD}\)                            : {token, {pseudo_class_odd, TokenLine}}.
\({EVEN}\)                           : {token, {pseudo_class_even, TokenLine}}.
\({PSEUDO_PATTERN}\)                 : {token, {pseudo_class_pattern, TokenLine, remove_wrapper(TokenChars)}}.
\({QUOTED}\)                         : {token, {pseudo_class_quoted, TokenLine, remove_wrapper(remove_wrapper(TokenChars))}}.
{W}*\)                               : {token, {close_parentheses, TokenLine}}.
~=                                   : {token, {includes, TokenLine}}.
\|=                                  : {token, {dash_match, TokenLine}}.
\^=                                  : {token, {prefix_match, TokenLine}}.
\$=                                  : {token, {suffix_match, TokenLine}}.
\*=                                  : {token, {substring_match, TokenLine}}.
=                                    : {token, {'=', TokenLine}}.
{W}*,{W}*                            : {token, {',', TokenLine}}.
{W}*>{W}*                            : {token, {'>', TokenLine}}.
{W}*\+{W}*                           : {token, {'+', TokenLine}}.
{W}*~{W}*                            : {token, {'~', TokenLine}}.
{W}*\|{W}*                           : {token, {'|', TokenLine}}.
{W}+                                 : {token, {' ', TokenLine}}.
.                                    : {token, {unknown, TokenLine, TokenChars}}.

Erlang code.

remove_wrapper(Chars) ->
  Len = string:len(Chars),
  string:substr(Chars, 2, Len - 2).

tail([_|T]) ->
  T.
