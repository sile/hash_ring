%% @copyright 2013-2016 Takeru Ohta <phjgt308@gmail.com>
%%
-module(hash_ring_node_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
make_test() ->
    Node = hash_ring_node:make(foo),
    ?assert(hash_ring_node:is_node(Node)),

    ?assertEqual(Node, hash_ring_node:make(foo, foo)),
    ?assertEqual(Node, hash_ring_node:make(foo, foo, [{weight, 1}])).

get_test() ->
    Node = hash_ring_node:make(foo, bar, [{weight, 1.5}]),
    ?assertEqual(foo, hash_ring_node:get_key(Node)),
    ?assertEqual(bar, hash_ring_node:get_data(Node)),
    ?assertEqual(1.5, hash_ring_node:get_weight(Node)).
