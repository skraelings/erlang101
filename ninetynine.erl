%% Ninety-Nine Prolog problems in Erlang
%% https://sites.google.com/site/prologsite/prolog-problems
%% dom 26 jun 2011 23:08:32 PET

-module(ninetynine).
-export([p1/1,p2/1,p3/2,p4/1,p5/1,p6/1,p7/1,p8/1]).

%% Find the last element of a list.
p1([Last]) ->
    Last;
p1([_|T]) ->
    p1(T).

%% Find the last but one element of a list.
p2([ButLast,_]) ->
    ButLast;
p2([_|T]) ->
    p2(T).

%% Find the K'th element of a list. The first element in the list is number 1.
p3([H|_], 1) ->
    H;
p3([_|T], K) ->
    p3(T, K-1).

%% Find the number of elements of a list.
p4(List) ->
    p4_accum(List, 0).
p4_accum([], Accum) ->
    Accum;
p4_accum([_|T], Accum) ->
    p4_accum(T, Accum+1).

%% Reverse a list.
p5(List) ->
    p5_aux(List, []).
p5_aux([], Reversed) ->
    Reversed;
p5_aux([H|T], Reversed) ->
    p5_aux(T, [H|Reversed]).

%% Find out whether a list is a palindrome.
p6(List) ->
    List == p5(List).

%% Flatten a nested list structure.
%%% Exhibits same behaviour as lists:flatten when elements are strings and
%%% lists:append won't work for very nested list.
%%% Use of io_lib:char_list as Peer Stritzinger suggest might be the solution:
%%% http://stackoverflow.com/questions/2911420/erlang-flattening-a-list-of-strings
p7(Nested_list) ->
    p5(p7_aux(Nested_list, [])).
p7_aux([], Flattened) ->
    Flattened;
p7_aux([H|T], Flattened) ->
    Flat = p7_aux(H, Flattened),
    p7_aux(T, Flat);
p7_aux(Val, Flattened) when not(is_list(Val)) ->
    [Val|Flattened].

%% Eliminate consecutive duplicates of list elements
p8(List) ->
    [H|T] = List,
    p8_aux(H, T, []).
p8_aux(H, [], Acc) ->
    lists:reverse([H|Acc]);
p8_aux(H, [H|T], Acc) ->
    p8_aux(H,T, Acc);
p8_aux(H, [H1|T], Acc) ->
    p8_aux(H1, T, [H|Acc]).

%% Pack consecutive duplicates of list elements into sublists
p9(List) ->
    null.

%% Run-length encoding of a list
p10(List) ->
    null.

-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual($d, p1([$a,$b,$c,$d])),
    13 = p1([1,3,7,13]).
p2_test() ->
    ?assertEqual($c, p2([$a,$b,$c,$d])),
    7 = p2([1,3,7,13]).
p3_test() ->
    ?assertEqual($E, p3("Erlang", 1)).
p4_test() ->
    ?assertEqual(6, p4([1,2,3,4,5,6])),
    ?assertEqual(6, p4("Erlang")),
    ?assertEqual(0, p4([])).
p5_test() ->
    ?assertEqual("gnalrE", p5("Erlang")),
    ?assertEqual("", p5("")).
p6_test() ->
    ?assertEqual(true, p6([$x,$a,$m,$a,$x])),
    ?assertEqual(true, p6([3,7,8,8,7,3])),
    ?assertEqual(false, p6([1,0,0])).
p7_test() ->
    ?assertEqual([$a,$b,$c,$d,$e], p7([$a,[$b,[$c,$d],$e]])),
    ?assertEqual([1,2,3,4], p7([[[1,2],[3]],[[4]]])).
p8_test() ->
    ?assertEqual([a,b,c,a,d,e], p8([a,a,a,a,b,c,c,a,a,d,e,e,e,e])).
