%% @copyright 2013-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc 動的なノード追加・削除に強いhash_ringの実装モジュール
-module(hash_ring_dynamic).

-compile(inline).
-behaviour(hash_ring).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([
         make/2,
         is_ring/1,
         add_nodes/2,
         remove_nodes/2,
         get_nodes/1,
         get_node_count/1,
         fold/4
        ]).

-export_type([
              ring/0,
              option/0
             ]).

%%--------------------------------------------------------------------------------
%% Macros & Recors & Types
%%--------------------------------------------------------------------------------
-define(RING, ?MODULE).
-define(DEFAULT_VIRTUAL_NODE_COUNT, 1024). % 各ノードごとの仮想ノードの数
-define(DEFAULT_MAX_HASH_BYTE_SIZE, 4).
-define(DEFAULT_HASH_ALGORITHM, md5).
-define(DEFAULT_WEIGHT_MODE, direct).

-record(?RING,
        {
          virtual_node_count   :: pos_integer(),
          vnodes               :: vnodes(),
          nodes                :: [hash_ring:ring_node()],
          node_count           :: non_neg_integer(),
          available_node_count :: non_neg_integer(),
          hash_mask            :: integer(),
          hash_algorithm       :: hash_ring:hash_algorithms(),
          weight_mode          :: weight_mode()
        }).

-opaque ring() :: #?RING{}.

-type option() :: {virtual_node_count, pos_integer()}
                | {max_hash_byte_size, pos_integer()}
                | {hash_algorithm, hash_ring:hash_algorithms()}
                | {weight_mode, weight_mode()}. % default: direct

-type weight_mode() :: direct | average.
%% - direct: 指定された重みをそのまま使う
%% - average: 各ノードの重みを平均化する

-type vnode_key() :: {Hash::non_neg_integer(), sequence(), hash_ring_node:key()}.
-type vnode_value() :: hash_ring:ring_node().
-type vnodes() :: gb_trees:tree(vnode_key(), vnode_value()).
-type sequence() :: non_neg_integer() | -1. % `-1' is the sentinel value

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
%% @doc コンシステントハッシュリングを構築する
-spec make([hash_ring:ring_node()], [option()]) -> ring().
make(Nodes, Options) ->
    VirtualNodeCount = proplists:get_value(virtual_node_count, Options, ?DEFAULT_VIRTUAL_NODE_COUNT),
    HashAlgorithm    = proplists:get_value(hash_algorithm, Options, ?DEFAULT_HASH_ALGORITHM),
    WeightMode       = proplists:get_value(weight_mode, Options, ?DEFAULT_WEIGHT_MODE),
    MaxHashByteSize0 = proplists:get_value(max_hash_byte_size, Options, ?DEFAULT_MAX_HASH_BYTE_SIZE),
    MaxHashByteSize  = min(MaxHashByteSize0, hash_ring_util:hash_byte_size(HashAlgorithm)),

    HashMask = (1 bsl MaxHashByteSize * 8) - 1,

    Ring =
        #?RING{
            virtual_node_count   = VirtualNodeCount,
            vnodes               = gb_trees:empty(),
            nodes                = [],
            node_count           = 0,
            available_node_count = 0,
            hash_mask            = HashMask,
            hash_algorithm       = HashAlgorithm,
            weight_mode          = WeightMode
           },
    add_nodes(Nodes, Ring).

%% @doc 引数の値が適切に生成されたリングオブジェクトかどうかを判定する
-spec is_ring(ring() | term()) -> boolean().
is_ring(#?RING{}) -> true;
is_ring(_)        -> false.

%% @doc ノード群を追加する
-spec add_nodes([hash_ring:ring_node()], ring()) -> ring().
add_nodes(Nodes, Ring) ->
    #?RING{vnodes = VirtualNodes0, nodes = Nodes0} = Ring,

    UniqNodes = lists:usort(fun node_less_than_or_equal_to/2, Nodes),

    VirtualNodes1 =
        lists:foldl(fun (Node, Acc) -> add_node(Node, Acc, Ring) end,
                    VirtualNodes0,
                    UniqNodes),
    Nodes1 = lists:umerge(fun node_less_than_or_equal_to/2, UniqNodes, Nodes0),

    Ring#?RING{
            vnodes               = VirtualNodes1,
            nodes                = Nodes1,
            node_count           = length(Nodes1),
            available_node_count = length(lists:filter(fun hash_ring_node:is_available/1, Nodes1))
           }.

%% @doc ノード群を削除する
-spec remove_nodes([hash_ring_node:key()], ring()) -> ring().
remove_nodes(Keys, Ring) ->
    #?RING{vnodes = VirtualNodes0, nodes = Nodes0} = Ring,
    KeySet = gb_sets:from_list(Keys),

    VirtualNodes1 =
        gb_sets:fold(
          fun (Key, Acc) -> remove_virtual_nodes(Key, Acc, 0, Ring) end,
          VirtualNodes0,
          KeySet),
    Nodes1 =
        lists:filter(fun (N) -> not gb_sets:is_member(hash_ring_node:get_key(N), KeySet) end, Nodes0),

    Ring#?RING{
            vnodes               = VirtualNodes1,
            nodes                = Nodes1,
            node_count           = length(Nodes1),
            available_node_count = length(lists:filter(fun hash_ring_node:is_available/1, Nodes1))
           }.

%% @doc ノード一覧を取得する
-spec get_nodes(ring()) -> [hash_ring:ring_node()].
get_nodes(Ring) ->
    Ring#?RING.nodes.

%% @see hash_ring:get_node_count/1
-spec get_node_count(ring()) -> non_neg_integer().
get_node_count(Ring) ->
    Ring#?RING.node_count.

%% @doc アイテムの次に位置するノードから順に畳み込みを行う
-spec fold(hash_ring:fold_fun(), hash_ring:item(), term(), ring()) -> Result::term().
fold(Fun, Item, Initial, Ring) ->
    #?RING{available_node_count = NodeCount, vnodes = VirtualNodes} = Ring,
    ItemHash = hash(Item, Ring),
    Position = find_start_position(ItemHash, VirtualNodes),
    fold_successor_nodes(NodeCount, Position, VirtualNodes, Fun, Initial).

%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec find_start_position(term(), vnodes()) -> gb_trees:iter(vnode_key(), vnode_value()).
find_start_position(ItemHash, VirtualNodes) ->
    VnodeKey = {ItemHash, -1, undefined},
    gb_trees:iterator_from(VnodeKey, VirtualNodes).

-spec fold_successor_nodes(non_neg_integer(), gb_trees:iter(vnode_key(), vnode_value()),
                           vnodes(), hash_ring:fold_fun(), term()) -> term().
fold_successor_nodes(RestNodeCount, StartPosition, VirtualNodes, Fun, Initial) ->
    fold_successor_nodes(RestNodeCount, StartPosition, VirtualNodes, Fun, Initial, gb_sets:empty()).

-spec fold_successor_nodes(non_neg_integer(), gb_trees:iter(vnode_key(), vnode_value()),
                           vnodes(), hash_ring:fold_fun(), term(), gb_sets:set(hash_ring:ring_node())) -> term().
fold_successor_nodes(0, _, _, _, Acc, _) ->
    Acc;
fold_successor_nodes(RestNodeCount, Position, VirtualNodes, Fun, Acc, IteratedNodes) ->
    case gb_trees:next(Position) of
        none ->
            Head = gb_trees:iterator(VirtualNodes),
            fold_successor_nodes(RestNodeCount, Head, VirtualNodes, Fun, Acc, IteratedNodes);
        {_, Node, Next} ->
            case gb_sets:is_member(Node, IteratedNodes) of
                true  -> fold_successor_nodes(RestNodeCount, Next, VirtualNodes, Fun, Acc, IteratedNodes);
                false ->
                    case Fun(Node, Acc) of
                        {false, Acc2} -> Acc2;
                        {true,  Acc2} ->
                            fold_successor_nodes(RestNodeCount - 1, Next, VirtualNodes, Fun, Acc2,
                                                 gb_sets:add(Node, IteratedNodes))
                    end
            end
    end.

-spec add_node(hash_ring:ring_node(), vnodes(), ring()) -> vnodes().
add_node(Node, VirtualNodes, Ring) ->
    #?RING{nodes = ExistingNodes, virtual_node_count = BaseCount} = Ring,
    VirtualNodeCount = hash_ring_node:calc_virtual_node_count(BaseCount, Node),
    case node_find(hash_ring_node:get_key(Node), ExistingNodes) of
        error     -> add_virtual_nodes(Node, VirtualNodes, 0, VirtualNodeCount, Ring);
        {ok, Old} ->
            OldCount = hash_ring_node:calc_virtual_node_count(BaseCount, Old),
            case OldCount =< VirtualNodeCount of
                true  -> add_virtual_nodes(Node, VirtualNodes, OldCount, VirtualNodeCount, Ring);
                false -> remove_virtual_nodes(hash_ring_node:get_key(Node), VirtualNodes, VirtualNodeCount, Ring)
            end
    end.

-spec add_virtual_nodes(hash_ring:ring_node(), vnodes(), sequence(), sequence(), ring()) -> vnodes().
add_virtual_nodes(Node, VirtualNodes, Start, End, Ring) ->
    Key = hash_ring_node:get_key(Node),
    lists:foldl(
      fun (Seq, Acc) ->
              VnodeKey = {hash({Seq, Key}, Ring), Seq, Key},
              gb_trees:insert(VnodeKey, Node, Acc)
      end,
      VirtualNodes,
      lists:seq(Start, End - 1)).

-spec remove_virtual_nodes(hash_ring_node:key(), vnodes(), sequence(), ring()) -> vnodes().
remove_virtual_nodes(Key, VirtualNodes, Seq, Ring) ->
    Hash = hash({Seq, Key}, Ring),
    VnodeKey = {Hash, Seq, Key},
    case gb_trees:lookup(VnodeKey, VirtualNodes) of
        none       -> VirtualNodes;
        {value, _} -> remove_virtual_nodes(Key, gb_trees:delete(VnodeKey, VirtualNodes), Seq + 1, Ring)
    end.

-spec node_find(hash_ring_node:key(), [hash_ring:ring_node()]) -> {ok, hash_ring:ring_node()} | error.
node_find(_Key, [])     -> error;
node_find(Key, [H | T]) ->
    case Key =:= hash_ring_node:get_key(H) of
        true  -> {ok, H};
        false -> node_find(Key, T)
    end.

-spec node_less_than_or_equal_to(hash_ring:ring_node(), hash_ring:ring_node()) -> boolean().
node_less_than_or_equal_to(A, B) ->
    hash_ring_node:get_key(A) =< hash_ring_node:get_key(B).

-spec hash(term(), ring()) -> non_neg_integer().
hash(X, Ring) ->
    #?RING{hash_mask = HashMask, hash_algorithm = HashAlgorithm} = Ring,
    hash_ring_util:calc_hash(HashAlgorithm, X) band HashMask.
