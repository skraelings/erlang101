%% Nine-Nine Lisp problems in Erlang
%% http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
%% dom 26 jun 2011 23:08:32 PET

-module(ninetynine).
-export([p1/1]).

%% Find the last box of a list.
p1([_,Last]) ->
    [Last];
p1([_|T]) ->
    p1(T).

%%


-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual([$d],p1([$a,$b,$c,$d])),
    [13] = p1([1,3,7,13]).
