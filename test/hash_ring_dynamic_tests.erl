%% @copyright 2013-2016 Takeru Ohta <phjgt308@gmail.com>
%%
-module(hash_ring_dynamic_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(MAKE_RING(Nodes),
        hash_ring:make(Nodes, [{module, hash_ring_dynamic},
                               {virtual_node_count, 128}])).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
make_test_() ->
    [
     {"Creates a empty ring",
      fun () ->
              Ring = ?MAKE_RING(list_to_nodes([])),
              ?assert(hash_ring:is_ring(Ring)),
              ?assertEqual([], hash_ring:get_node_list(Ring)),
              ?assertEqual(0, hash_ring:get_node_count(Ring))
      end},
     {"Creates a non-empty ring",
      fun () ->
              Nodes = list_to_nodes([a, b, c]),
              Ring = ?MAKE_RING(Nodes),
              ?assert(hash_ring:is_ring(Ring)),
              ?assertEqual(Nodes, hash_ring:get_node_list(Ring)),
              ?assertEqual(3, hash_ring:get_node_count(Ring))
      end}
    ].

add_nodes_test_() ->
    [
     {"構築後にノードの追加が可能",
      fun () ->
              Nodes = list_to_nodes(lists:seq(1, 100)),
              Ring0 = ?MAKE_RING(Nodes),
              Ring1 = lists:foldl(fun hash_ring:add_nodes/2, ?MAKE_RING([]), [[N] || N <- Nodes]),
              ?assertEqual(hash_ring:get_node_list(Ring0), hash_ring:get_node_list(Ring1))
      end},
     {"追加後に重みの変更が可能",
      fun () ->
              Node0 = hash_ring_node:make(a, a, [{weight, 1}]),
              Ring0 = ?MAKE_RING([Node0]),

              ?assertEqual({ok, Node0}, hash_ring:find_node(hoge, Ring0)),

              %% weight: 1 => 0
              %% - 仮想ノード数が0になる
              Node1 = hash_ring_node:make(a, a, [{weight, 0}]),
              Ring1 = hash_ring:add_nodes([Node1], Ring0),
              ?assertEqual(error, hash_ring:find_node(hoge, Ring1)),
              ?assertEqual([Node1], hash_ring:get_node_list(Ring1)), % ノードの登録はされている

              %% weight: 0 => 1
              Ring2 = hash_ring:add_nodes([Node0], Ring1),
              ?assertEqual({ok, Node0}, hash_ring:find_node(hoge, Ring2))
      end}
    ].

remove_nodes_test_() ->
    [
     {"構築後にノードの削除が可能",
      fun () ->
              Nodes = list_to_nodes(lists:seq(1, 100)),
              Ring0 = ?MAKE_RING([]),
              Ring1 = lists:foldl(fun hash_ring:remove_nodes/2, ?MAKE_RING(Nodes),
                                  [[hash_ring_node:get_key(N)] || N <- Nodes]),
              ?assertEqual(Ring0, Ring1)
      end}
    ].

find_test_() ->
    [
     {"リングが空の場合は'error'が返される",
      fun () ->
              Ring = ?MAKE_RING(list_to_nodes([])),
              ?assertEqual(error, hash_ring:find_node(hoge, Ring))
      end},
     {"ノード数が1の場合",
      fun () ->
              Ring = ?MAKE_RING(list_to_nodes([a])),

              ?assertMatch({ok, _}, hash_ring:find_node(hoge, Ring)),
              {ok, Node} = hash_ring:find_node(hoge, Ring),

              ?assertEqual(a, hash_ring_node:get_key(Node))
      end},
     {"ノード数が1以上の場合",
      fun () ->
              Ring = ?MAKE_RING(list_to_nodes(lists:seq(1, 100))),
              ?assertMatch({ok, _}, hash_ring:find_node(hoge, Ring)) % 何が返ってくるかは不明
      end}
    ].

collect_nodes_test_() ->
    [
     {"指定数だけノードを集める",
      fun () ->
              Ring = ?MAKE_RING(list_to_nodes(lists:seq(1, 100))),
              ?assertMatch([],     hash_ring:collect_nodes(hoge, 0, Ring)),
              ?assertMatch([_],    hash_ring:collect_nodes(hoge, 1, Ring)),
              ?assertMatch([_, _], hash_ring:collect_nodes(hoge, 2, Ring))
      end}
    ].


fold_test_() ->
    [
     {"空リングに対するfoldは初期値がそのまま返される",
      fun () ->
              Ring = ?MAKE_RING(list_to_nodes([])),
              ?assertEqual(initial, hash_ring:fold(fun (_, _) -> {false, fuga} end, hoge, initial, Ring))
      end},
     {"任意の箇所で畳み込みを中断できる",
      fun () ->
              Ring = ?MAKE_RING(list_to_nodes(lists:seq(1, 100))),

              Limit = 3,
              Fun = fun (Node, {Acc, Count}) -> {Count < Limit, {[Node | Acc], Count + 1}} end, % Limitに達するまでノードを集める

              ?assertMatch({[_, _, _], _}, hash_ring:fold(Fun, hoge, {[], 1}, Ring))
      end},
     {"全ノードを走査した時点で畳み込みは終了する",
      fun () ->
              Ring = ?MAKE_RING(list_to_nodes([a, b, c, d, e, f, g])),

              Limit = 10,
              Fun = fun (Node, {Acc, Count}) -> {Count < Limit, {[Node | Acc], Count + 1}} end,

              ?assertMatch({[_, _, _, _, _, _, _], _}, % length(Nodes) < Limit なので、Limitに達する前に畳み込みは終了する
                           hash_ring:fold(Fun, hoge, {[], 1}, Ring))
      end}
    ].

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec list_to_nodes([hash_ring_node:key()]) -> [hash_ring_node:ring_node()].
list_to_nodes(Keys) ->
    lists:map(fun hash_ring_node:make/1, Keys).
