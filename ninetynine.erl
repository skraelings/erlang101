%% Nine-Nine Lisp problems in Erlang
%% https://sites.google.com/site/prologsite/prolog-problems
%% dom 26 jun 2011 23:08:32 PET

-module(ninetynine).
-export([p1/1,p2/1,p3/2,p4/1]).

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
    p3(T,K-1).

%% Find the number of elements of a list.
p4([_|T]) ->
    p4_counter(T,1).
p4_counter([], Counter) ->
    Counter;
p4_counter([_|T], Counter) ->
    p4_counter(T, Counter+1).

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
    ?assertEqual(6, p4("Erlang")).
