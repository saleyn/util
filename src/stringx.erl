%%%------------------------------------------------------------------------
%%% @doc Implements miscelaneous string functions
%%%
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @copyright 2006 Serge Aleynikov
%%% @end
%%% Some implementation authored by Evan Miller:
%%% http://www.evanmiller.org/joy-of-erlang.html
%%%------------------------------------------------------------------------
%%% Created 2005-09-25
%%%------------------------------------------------------------------------
-module(stringx).
-author('saleyn@gmail.com').

%% External API
-export([titlecase/1, wordcount/1, wordwrap/2]).

%%%------------------------------------------------------------------------
%%% External API
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc Convert words in a string to capitalize first letter of each word.
%% @end
%%-------------------------------------------------------------------------
-spec titlecase(string()) -> string().
titlecase(S) when is_list(S) ->
    titlecase(S, []).

%%-------------------------------------------------------------------------
%% @doc Count number of words in a string
%% @end
%%-------------------------------------------------------------------------
-spec wordcount(string()) -> string().
wordcount(S) ->
    wordcount(S, 0).

%%-------------------------------------------------------------------------
%% @doc Count number of words in a string
%% @end
%%-------------------------------------------------------------------------
-spec wordwrap(string(), integer()) -> string().
wordwrap(S, Margin) when is_list(S), is_integer(Margin) ->
    wordwrap(S, [], [], 0, Margin).

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

titlecase([], Acc) ->
    lists:reverse(Acc);
titlecase([C | T], [] = Acc) when C >= $a, C =< $z ->
    titlecase(T, [C + ($A - $a) | Acc]);
titlecase([C | T], [$  |_] = Acc) when C >= $a, C =< $z ->
    titlecase(T, [C + ($A - $a) | Acc]);
titlecase([C | T], Acc) ->
    titlecase(T, [C | Acc]).

wordcount([],  N)                    -> N;
wordcount([C], N) when C =/= $       -> N+1;
wordcount([C,$ |T], N) when C =/= $  -> wordcount([$ |T], N+1);
wordcount([_   |T], N)               -> wordcount(T, N).

wordwrap([], Acc, WordAcc, _LineLen, _Margin) ->
    lists:reverse(WordAcc ++ Acc);

% Premature newline
wordwrap([$\n | Rest], Acc, WordAcc, _LineLen, Margin) ->
    wordwrap(Rest, [$\n | WordAcc ++ Acc], [], 0, Margin);

% Hit the wrap length at a space character. Add a newline
wordwrap([$  | Rest], Acc, WordAcc, Margin, Margin) ->
    wordwrap(Rest, [$\n | WordAcc ++ Acc], [], 0, Margin);

% Hit a space character before the wrap length. Keep going
wordwrap([$  | Rest], Acc, WordAcc, LineLen, Margin) ->
    wordwrap(Rest, [$  | WordAcc ++ Acc], [], LineLen + 1 + length(WordAcc), Margin);

% Overflowed the current line while building a word
wordwrap([C | Rest], Acc, WordAcc, 0, Margin) when erlang:length(WordAcc) > Margin ->
    wordwrap(Rest, Acc, [C | WordAcc], 0, Margin);
wordwrap([C | Rest], Acc, WordAcc, LineLen, Margin) 
    when erlang:length(WordAcc) + LineLen > Margin ->
    wordwrap(Rest, [$\n | Acc], [C | WordAcc], 0, Margin);

% Just building a word...
wordwrap([C | Rest], Acc, WordAcc, LineLen, Margin) ->
    wordwrap(Rest, Acc, [C | WordAcc], LineLen, Margin).
