%% @copyright 2013-2016 Takeru Ohta <phjgt308@gmail.com>
%%
-module(hash_ring_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(MAKE_RING(Nodes),
        hash_ring:make(Nodes, [{module, hash_ring_static},
                               {sentinel_key, 'SENTINEL'},
                               {virtual_node_count, 128}])).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
add_test_() ->
    [
     {"Add nodes",
      fun () ->
              Ring0 = ?MAKE_RING([]),
              Nodes = [hash_ring_node:make(I) || I <- lists:seq(1, 5)],
              Ring1_A = hash_ring:add_nodes(Nodes, Ring0),
              Ring1_B = lists:foldl(fun hash_ring:add_node/2, Ring0, Nodes),
              ?assertEqual(Ring1_A, Ring1_B),
              ?assertEqual(lists:sort(Nodes), lists:sort(hash_ring:get_nodes(Ring1_A)))
      end}
    ].

remove_test_() ->
    [
     {"Remove nodes",
      fun () ->
              Keys = lists:seq(1, 5),
              Nodes = [hash_ring_node:make(I) || I <- Keys],
              Ring0 = ?MAKE_RING(Nodes),

              Ring1_A = hash_ring:remove_nodes(Keys, Ring0),
              Ring1_B = lists:foldl(fun hash_ring:remove_node/2, Ring0, Keys),
              ?assertEqual(Ring1_A, Ring1_B),
              ?assertEqual([], hash_ring:get_nodes(Ring1_A))
      end}
    ].

list_to_nodes_test_() ->
    [
     {"Creates a list of node",
      fun () ->
              Nodes0 = hash_ring:list_to_nodes([a, {b, 1}, {c, 2, [{weight, 0.2}]}]),
              Nodes1 = [hash_ring_node:make(a), hash_ring_node:make(b, 1), hash_ring_node:make(c, 2, [{weight, 0.2}])],
              ?assertEqual(Nodes0, Nodes1)
      end}
    ].
