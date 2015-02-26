-module(ninetynine_binary_trees).
-export([new_tree/3,
	 is_tree/1]).


%% returns a tree with root element as Root and its two succesors LeftChild and RightChild
new_tree(nil, nil, nil) ->
    nil;
new_tree(Root, LeftChild, RightChild) ->
    {Root, LeftChild, RightChild}.

%% Problem 4.01: Check whether a given term represents a binary tree
%%
%% is_tree checks wether the passed value T is a tree or not.
%% For T to be a tree her LeftChild and RightChild have to be a tree as well.
is_tree(T) ->
    is_tree_impl(T).

is_tree_impl(nil) ->
    true;
is_tree_impl({_, LeftChild, RightChild}) ->
    is_tree_impl(LeftChild) and is_tree_impl(RightChild);
is_tree_impl(_) ->
    false.

%% tests
-include_lib("eunit/include/eunit.hrl").

%% Test problem 4.01
is_tree_test() ->
    ?assertEqual(true, is_tree(new_tree(a, nil, nil))),
    ?assertEqual(true, is_tree(new_tree(nil,nil,nil))),
    ?assertEqual(true, is_tree(new_tree(b, new_tree(a, nil, nil), new_tree(c, new_tree(d, nil, nil), nil)))),
    ?assertEqual(false, is_tree(new_tree(a, new_tree(b, nil, nil), d))),
    ?assertEqual(true, is_tree(new_tree("root", new_tree("right", nil, nil), new_tree("left",nil,nil)))),
    ?assertEqual(true, is_tree(new_tree(new_tree(a, nil, nil), nil, nil))).
