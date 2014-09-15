%% coding: latin-1
%% @copyright 2013-2014 Takeru Ohta <phjgt308@gmail.com>
%%
-module(hash_ring_static_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(MAKE_STATIC_RING(Nodes), hash_ring:make(Nodes, [{module, hash_ring_static},
                                                        {sentinel_node, 'SENTINEL'},
                                                        {virtual_node_count, 128}])).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
make_test_() ->
    [
     {"Creates a empty ring",
      fun () ->
              Ring = ?MAKE_STATIC_RING([]),
              ?assert(hash_ring:is_ring(Ring)),
              ?assertEqual([], hash_ring:get_nodes(Ring))
      end},
     {"Creates a non-empty ring",
      fun () ->
              Nodes = [a, b, c],
              Ring = ?MAKE_STATIC_RING(Nodes),
              ?assert(hash_ring:is_ring(Ring)),
              ?assertEqual(Nodes, hash_ring:get_nodes(Ring))
      end}
    ].

add_nodes_test_() ->
    [
     {"構築後にノードの追加が可能",
      fun () ->
              Nodes = lists:seq(1, 100),
              Ring0 = ?MAKE_STATIC_RING(Nodes),
              Ring1 = lists:foldl(fun hash_ring:add_nodes/2, ?MAKE_STATIC_RING([]), [[N] || N <- Nodes]),
              ?assertEqual(Ring0, Ring1)
      end}
    ].

remove_nodes_test_() ->
    [
     {"構築後にノードの削除が可能",
      fun () ->
              Nodes = lists:seq(1, 100),
              Ring0 = ?MAKE_STATIC_RING([]),
              Ring1 = lists:foldl(fun hash_ring:remove_nodes/2, ?MAKE_STATIC_RING(Nodes), [[N] || N <- Nodes]),
              ?assertEqual(Ring0, Ring1)
      end}
    ].

find_test_() ->
    [
     {"リングが空の場合は'error'が返される",
      fun () ->
              Ring = ?MAKE_STATIC_RING([]),
              ?assertEqual(error, hash_ring:find_node(hoge, Ring))
      end},
     {"ノード数が1の場合",
      fun () ->
              Ring = ?MAKE_STATIC_RING([a]),
              ?assertEqual({ok, a}, hash_ring:find_node(hoge, Ring))
      end},
     {"ノード数が1以上の場合",
      fun () ->
              Ring = ?MAKE_STATIC_RING(lists:seq(1, 100)),
              ?assertMatch({ok, _}, hash_ring:find_node(hoge, Ring)) % 何が返ってくるかは不明
      end}
    ].

fold_test_() ->
    [
     {"空リングに対するfoldは初期値がそのまま返される",
      fun () ->
              Ring = ?MAKE_STATIC_RING([]),
              ?assertEqual(initial, hash_ring:fold(fun (_, _) -> {false, fuga} end, hoge, initial, Ring))
      end},
     {"任意の箇所で畳み込みを中断できる",
      fun () ->
              Ring = ?MAKE_STATIC_RING(lists:seq(1, 100)),

              Limit = 3,
              Fun = fun (Node, {Acc, Count}) -> {Count < Limit, {[Node | Acc], Count + 1}} end, % Limitに達するまでノードを集める

              ?assertMatch({[_, _, _], _}, hash_ring:fold(Fun, hoge, {[], 1}, Ring))
      end},
     {"全ノードを走査した時点で畳み込みは終了する",
      fun () ->
              Ring = ?MAKE_STATIC_RING([a, b, c, d, e, f, g]),

              Limit = 10,
              Fun = fun (Node, {Acc, Count}) -> {Count < Limit, {[Node | Acc], Count + 1}} end,

              ?assertMatch({[_, _, _, _, _, _, _], _}, % length(Nodes) < Limit なので、Limitに達する前に畳み込みは終了する
                           hash_ring:fold(Fun, hoge, {[], 1}, Ring))
      end}
    ].
