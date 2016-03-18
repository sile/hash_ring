%% @copyright 2013-2016 Takeru Ohta <phjgt308@gmail.com>
%%
-module(hash_ring_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(MAKE_RING(Module, Nodes), hash_ring:make(Nodes, [{module, Module}, {virtual_node_count, 128}])).
-define(MODULES, [hash_ring_static, hash_ring_dynamic]).

%%----------------------------------------------------------------------------------------------------------------------
%% Test Cases
%%----------------------------------------------------------------------------------------------------------------------
make_test_() ->
    lists:append(lists:map(fun gen_make_test/1, ?MODULES)).

add_nodes_test_() ->
    lists:append(lists:map(fun gen_add_nodes_test/1, ?MODULES)).

remove_nodes_test_() ->
    lists:append(lists:map(fun gen_remove_nodes_test/1, ?MODULES)).

find_test_() ->
    lists:append(lists:map(fun gen_find_test/1, ?MODULES)).

collect_nodes_test_() ->
    lists:append(lists:map(fun gen_collect_nodes_test/1, ?MODULES)).

fold_test_() ->
    lists:append(lists:map(fun gen_fold_test/1, ?MODULES)).

list_to_nodes_test_() ->
    [
     {"Creates a list of node",
      fun () ->
              Nodes0 = hash_ring:list_to_nodes([a, {b, 1}, {c, 2, [{weight, 0.2}]}]),
              Nodes1 = [hash_ring_node:make(a), hash_ring_node:make(b, 1), hash_ring_node:make(c, 2, [{weight, 0.2}])],
              ?assertEqual(Nodes0, Nodes1)
      end}
    ].

misc_test_() ->
    [
     {"The difference of the implementation module does not affect the logical ring structure",
      fun () ->
              Nodes = hash_ring:list_to_nodes(lists:seq(1, 100)),
              Rings =
                  [?MAKE_RING(Module, Nodes) || Module <- ?MODULES],
              [?assertEqual(hash_ring:collect_nodes(foo, 100, A), hash_ring:collect_nodes(foo, 100, B))
               || A <- Rings, B <- Rings]
      end}
    ].

gen_make_test(Module) ->
    ModuleName = atom_to_list(Module),
    [
     {ModuleName ++ ": Creates a empty ring",
      fun () ->
              Ring = ?MAKE_RING(Module, hash_ring:list_to_nodes([])),
              ?assert(hash_ring:is_ring(Ring)),
              ?assertEqual([], hash_ring:get_node_list(Ring)),
              ?assertEqual(0, hash_ring:get_node_count(Ring))
      end},
     {ModuleName ++ ": Creates a non-empty ring",
      fun () ->
              Nodes = hash_ring:list_to_nodes([a, b, c]),
              Ring = ?MAKE_RING(Module, Nodes),
              ?assert(hash_ring:is_ring(Ring)),
              ?assertEqual(Nodes, hash_ring:get_node_list(Ring)),
              ?assertEqual(3, hash_ring:get_node_count(Ring))
      end}
    ].

gen_add_nodes_test(Module) ->
    ModuleName = atom_to_list(Module),
    [
     {ModuleName ++ ": Adds nodes dynamically",
      fun () ->
              Nodes = hash_ring:list_to_nodes(lists:seq(1, 100)),
              Ring0 = ?MAKE_RING(Module, Nodes),
              Ring1 = lists:foldl(fun hash_ring:add_node/2, ?MAKE_RING(Module, []), Nodes),
              ?assertEqual(hash_ring:get_node_list(Ring0), hash_ring:get_node_list(Ring1))
      end},
     {ModuleName ++ ": Changes the weight of a node",
      fun () ->
              Node0 = hash_ring_node:make(a, a, [{weight, 1}]),
              Ring0 = ?MAKE_RING(Module, [Node0]),

              ?assertEqual({ok, Node0}, hash_ring:find_node(foo, Ring0)),

              %%% weight: 1 => 0 (i.e., the virtual node count of the node becomes zero)
              Node1 = hash_ring_node:make(a, a, [{weight, 0}]),
              Ring1 = hash_ring:add_node(Node1, Ring0),

              %% The node exists in the ring but it is invisible from `hash_ring:find_node/2'
              ?assertEqual([Node1], hash_ring:get_node_list(Ring1)),
              ?assertEqual(error, hash_ring:find_node(foo, Ring1)),

              %%% weight: 0 => 1
              Ring2 = hash_ring:add_node(Node0, Ring1),
              ?assertEqual({ok, Node0}, hash_ring:find_node(foo, Ring2))
      end}
    ].

gen_remove_nodes_test(Module) ->
    ModuleName = atom_to_list(Module),
    [
     {ModuleName ++ ": Removes nodes dynamically",
      fun () ->
              Nodes = hash_ring:list_to_nodes(lists:seq(1, 100)),
              Ring0 = ?MAKE_RING(Module, []),
              Ring1 = lists:foldl(fun hash_ring:remove_node/2, ?MAKE_RING(Module, Nodes),
                                  [hash_ring_node:get_key(N) || N <- Nodes]),
              ?assertEqual(Ring0, Ring1)
      end}
    ].

gen_find_test(Module) ->
    ModuleName = atom_to_list(Module),
    [
     {ModuleName ++ ": Empty ring",
      fun () ->
              Ring = ?MAKE_RING(Module, hash_ring:list_to_nodes([])),
              ?assertEqual(error, hash_ring:find_node(foo, Ring))
      end},
     {ModuleName ++ ": The number of nodes is one",
      fun () ->
              Ring = ?MAKE_RING(Module, hash_ring:list_to_nodes([a])),

              ?assertMatch({ok, _}, hash_ring:find_node(foo, Ring)),
              {ok, Node} = hash_ring:find_node(foo, Ring),

              ?assertEqual(a, hash_ring_node:get_key(Node))
      end},
     {ModuleName ++ ": The number of nodes is more than one",
      fun () ->
              Ring = ?MAKE_RING(Module, hash_ring:list_to_nodes(lists:seq(1, 100))),
              ?assertMatch({ok, _}, hash_ring:find_node(foo, Ring)),
              {ok, Node} = hash_ring:find_node(foo, Ring),

              Key = hash_ring_node:get_key(Node),
              ?assert(1 =< Key andalso Key =< 100)
      end}
    ].

gen_collect_nodes_test(Module) ->
    ModuleName = atom_to_list(Module),
    [
     {ModuleName ++ ": Collects multiple nodes",
      fun () ->
              Ring = ?MAKE_RING(Module, hash_ring:list_to_nodes(lists:seq(1, 100))),
              ?assertMatch([],     hash_ring:collect_nodes(foo, 0, Ring)),
              ?assertMatch([_],    hash_ring:collect_nodes(foo, 1, Ring)),
              ?assertMatch([_, _], hash_ring:collect_nodes(foo, 2, Ring)),

              N1 = hash_ring:collect_nodes(foo, 1, Ring),
              N2 = hash_ring:collect_nodes(foo, 2, Ring),
              N4 = hash_ring:collect_nodes(foo, 4, Ring),
              N8 = hash_ring:collect_nodes(foo, 8, Ring),
              ?assert(lists:prefix(N1, N2)),
              ?assert(lists:prefix(N2, N4)),
              ?assert(lists:prefix(N4, N8))
      end}
    ].

gen_fold_test(Module) ->
    ModuleName = atom_to_list(Module),
    [
     {ModuleName ++ ": Empty ring",
      fun () ->
              Ring = ?MAKE_RING(Module, hash_ring:list_to_nodes([])),
              Initial = initial,
              ?assertEqual(Initial, hash_ring:fold(fun (_, _) -> {false, fuga} end, foo, Initial, Ring))
      end},
     {ModuleName ++ ": Breaks a folding",
      fun () ->
              Ring = ?MAKE_RING(Module, hash_ring:list_to_nodes(lists:seq(1, 100))),

              %% `Fun' collects nodes up to `Limit'
              Limit = 3,
              Fun = fun (Node, {Acc, Count}) -> {Count < Limit, {[Node | Acc], Count + 1}} end,

              ?assertMatch({[_, _, _], _}, hash_ring:fold(Fun, foo, {[], 1}, Ring))
      end},
     {ModuleName ++ ": A folding terminates when all nodes are traversed",
      fun () ->
              Ring = ?MAKE_RING(Module, hash_ring:list_to_nodes([a, b, c, d, e])),

              %% `Fun' collects nodes up to `Limit'
              Limit = 10,
              Fun = fun (Node, {Acc, Count}) -> {Count < Limit, {[Node | Acc], Count + 1}} end,

              %% The folding have terminated before the number of nodes collected reaches to `Limit'
              ?assertMatch({[_, _, _, _, _], _}, hash_ring:fold(Fun, foo, {[], 1}, Ring))
      end}
    ].
