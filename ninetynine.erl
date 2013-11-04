%% Ninety-Nine Prolog problems in Erlang
%% https://sites.google.com/site/prologsite/prolog-problems
%% dom 26 jun 2011 23:08:32 PET

-module(ninetynine).
-export([p1/1,p2/1,p3/2,p4/1,p5/1,
	 p6/1,p7/1,p8/1,p9/1,p10/1,
	 p11/1,p12/1,p13/1,p14/1]).

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
p9([]) ->
    [];
p9(List) ->
    [H|T] = List,
    p9_aux(H, T, []).
p9_aux(H, [], Acc) ->
    lists:reverse([H|Acc]);
p9_aux(H, [H|T], Acc) ->
    case p9_aux2(H, T, [H]) of
	[NewH, NewT, Duplicates] when is_list(Duplicates) ->
	    p9_aux(NewH, NewT, [Duplicates|Acc]);
	Duplicates ->
	    lists:reverse([Duplicates|Acc])
    end;
p9_aux(H, [H1|T], Acc) ->
    p9_aux(H1, T, [H|Acc]).

p9_aux2(H, [], DupStack) ->
    [H|DupStack];
p9_aux2(H, [H], DupStack) ->
    p9_aux2(H, [], [H|DupStack]);
p9_aux2(H, [H|T], DupStack) ->
    p9_aux2(H, T, [H|DupStack]);
p9_aux2(H, [H1|T], DupStack) ->
    [H1,T,[H|DupStack]].

%% Run-length encoding of a list
p10(List) ->
    [H|T] = p9(List),
    p10_aux(H, T, []).
p10_aux(H, [], Acc) when is_list(H) ->
    NE = [p4(H), hd(H)],
    lists:reverse([NE|Acc]);
p10_aux(H, [], Acc) ->
    NE = [1, H],
    lists:reverse([NE|Acc]);
p10_aux(H, [H1|T], Acc) when is_list(H) ->
    NE = [p4(H), hd(H)],
    p10_aux(H1, T, [NE|Acc]);
p10_aux(H, [H1|T], Acc) ->
    NE = [1, H],
    p10_aux(H1, T, [NE|Acc]).

%% Modified run-length encoding 
%% Modify the result of problem 1.10 in such a way that if an element has no
%% duplicates it is simply copied into the result list. Only elements with
%% duplicates are transferred as [N,E] terms
p11(List) ->
    [H|T] = p9(List),
    p11_aux(H, T, []).
p11_aux(H, [], Acc) when is_list(H) ->
    NE = [p4(H), hd(H)],
    lists:reverse([NE|Acc]);
p11_aux(H, [], Acc) ->
    lists:reverse([H|Acc]);
p11_aux(H, [H1|T], Acc) when is_list(H) ->
    NE = [p4(H), hd(H)],
    p11_aux(H1, T, [NE|Acc]);
p11_aux(H, [H1|T], Acc) ->
    p11_aux(H1, T, [H|Acc]).

%% Decode a run-length encoded list
p12(List) ->
    [H|T] = List,
    p12_aux(H, T, []).
p12_aux([N,E], [], Acc) ->
    %% create the list `E repeated N times`
    %% extend Acc with the list just created
    ExpandedList = p12_dup(E, N),
    lists:reverse(lists:append(ExpandedList, Acc));
p12_aux([N,E], [H|T], Acc) ->
    ExpandedList = p12_dup(E, N),
    p12_aux(H, T, lists:append(ExpandedList, Acc));
p12_aux(N, [], Acc) ->
    lists:reverse([N|Acc]);
p12_aux(N, [H|T], Acc) ->
    p12_aux(H, T, [N|Acc]).

p12_dup(E, N) ->
    p12_dup_aux(E, N - 1, []).
p12_dup_aux(E, 0, Acc) ->
    [E|Acc];
p12_dup_aux(E, N, Acc) ->
    p12_dup_aux(E, N - 1, [E|Acc]).

%% Run-length encoding of a list (direct solution).
p13(List) ->
    undefined.

%% Duplicate the elements of a list.
p14(List) ->
    p14_aux(List, []).
p14_aux([H|T], Acc) ->
    p14_aux(T, lists:append(p12_dup(H, 2), Acc));
p14_aux([], Acc) ->
    lists:reverse(Acc).

-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual($d, p1([$a,$b,$c,$d])),
    13 = p1([1,3,7,13]),
    ok.
p2_test() ->
    ?assertEqual($c, p2([$a,$b,$c,$d])),
    7 = p2([1,3,7,13]),
    ok.
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
p9_test() ->
    ?assertEqual([[a,a,a],b,c,[d,d,d],e,[f,f,f]], p9([a,a,a,b,c,d,d,d,e,f,f,f])).
p10_test() ->
    ?assertEqual([[4,a],[1,b],[2,c],[2,a],[1,d],[4,e]], p10([a,a,a,a,b,c,c,a,a,d,e,e,e,e])).
p11_test() ->
    ?assertEqual([[4,a],b,[2,c],[2,a],d,[4,e]], p11([a,a,a,a,b,c,c,a,a,d,e,e,e,e])).
p12_test() ->
    ?assertEqual([a,a,a,a,b,c,c,a,a,d,e,e,e,e], p12([[4,a],b,[2,c],[2,a],d,[4,e]])),
    ?assertEqual([a,a,a,a,b,c,c,a,a,d,e,e,e,e], p12([[4,a],[1,b],[2,c],[2,a],[1,d],[4,e]])).
p13_test() ->
    ?assertEqual([[4,a],b,[2,c],[2,a],d,[4,e]], p13([a,a,a,a,b,c,c,a,a,d,e,e,e,e])).
p14_test() ->
    ?assertEqual([a,a,b,b,c,c,c,c,d,d], p14([a,b,c,c,d])).
