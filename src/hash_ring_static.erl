%% @copyright 2013-2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc 静的に構築されたコンシステントハッシュリングを操作するためのモジュール
-module(hash_ring_static).

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
              option/0,
              key_fun/0
             ]).

%%--------------------------------------------------------------------------------
%% Macros & Recors & Types
%%--------------------------------------------------------------------------------
-define(RING, ?MODULE).
-define(DEFAULT_VIRTUAL_NODE_COUNT, 1024). % 各ノードごとの仮想ノードの数
-define(DEFAULT_MAX_HASH_BYTE_SIZE, 4).
-define(DEFAULT_HASH_ALGORITHM, md5).

-record(?RING,
        {
          virtual_node_count  :: pos_integer(),
          virtual_node_hashes :: tuple(),
          virtual_nodes       :: tuple(),
          nodes               :: [hash_ring:ring_node()],
          node_count          :: non_neg_integer(),
          hash_mask           :: integer(),
          hash_algorithm      :: hash_ring:hash_algorithms(),
          key_fun             :: key_fun()
        }).

-opaque ring() :: #?RING{}.

-type option() :: {virtual_node_count, pos_integer()}
                | {node_key_fun, key_fun()}
                | {max_hash_byte_size, pos_integer()}
                | {hash_algorithm, hash_ring:hash_algorithms()}
                | {sentinel_node, term()}. % for unit-test

-type key_fun() :: fun ((hash_ring:ring_node()) -> Key::term()).

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
%% @doc コンシステントハッシュリングを構築する
-spec make([hash_ring:ring_node()], [option()]) -> ring().
make(Nodes, Options) ->
    SentinelNode     = proplists:get_value(sentinel_node, Options, make_ref()),
    VirtualNodeCount = proplists:get_value(virtual_node_count, Options, ?DEFAULT_VIRTUAL_NODE_COUNT),
    HashAlgorithm    = proplists:get_value(hash_algorithm, Options, ?DEFAULT_HASH_ALGORITHM),
    MaxHashByteSize0 = proplists:get_value(max_hash_byte_size, Options, ?DEFAULT_MAX_HASH_BYTE_SIZE),
    MaxHashByteSize  = min(MaxHashByteSize0, hash_ring_util:hash_byte_size(HashAlgorithm)),

    HashMask = (1 bsl MaxHashByteSize * 8) - 1,

    Ring =
        #?RING{
            virtual_node_count  = VirtualNodeCount,
            virtual_node_hashes = {HashMask + 1}, % store sentinel value
            virtual_nodes       = {SentinelNode},
            nodes               = [],
            node_count          = 0,
            hash_mask           = HashMask,
            hash_algorithm      = HashAlgorithm,
            key_fun             = proplists:get_value(node_key_fun, Options, fun hash_ring_util:identity/1)
           },
    add_nodes(Nodes, Ring).

%% @doc 引数の値が適切に生成されたリングオブジェクトかどうかを判定する
-spec is_ring(ring() | term()) -> boolean().
is_ring(#?RING{}) -> true;
is_ring(_)        -> false.

%% @doc ノード群を追加する
-spec add_nodes([hash_ring:ring_node()], ring()) -> ring().
add_nodes(Nodes, Ring) ->
    #?RING{virtual_node_count = VirtualNodeCount, virtual_node_hashes = VirtualNodeHashes0, virtual_nodes = VirtualNodes0,
           nodes = Nodes0, key_fun = KeyFun} = Ring,

    Keys = lists:map(KeyFun, Nodes),
    VirtualNodes1 = [{hash({I, Key}, Ring), Node} || I <- lists:seq(1, VirtualNodeCount), {Key, Node} <- lists:zip(Keys, Nodes)],
    VirtualNodes2 = lists:sort(VirtualNodes1), % lists:keysort/2 だとハッシュ値に衝突がある場合に、順番が一意に定まらないので単なる sort/1 を使用する
    VirtualNodes3 = lists:umerge(VirtualNodes2, lists:zip(tuple_to_list(VirtualNodeHashes0), tuple_to_list(VirtualNodes0))),

    Nodes1 = lists:usort(Nodes0 ++ Nodes),

    Ring#?RING{
            virtual_node_hashes = list_to_tuple([Hash || {Hash, _} <- VirtualNodes3]),
            virtual_nodes       = list_to_tuple([Node || {_, Node} <- VirtualNodes3]),
            nodes               = Nodes1,
            node_count          = length(Nodes1)
           }.

%% @doc ノード群を削除する
-spec remove_nodes([hash_ring:ring_node()], ring()) -> ring().
remove_nodes(Nodes, Ring) ->
    #?RING{virtual_node_hashes = VirtualNodeHashes0, virtual_nodes = VirtualNodes0, nodes = Nodes0} = Ring,
    NodeSet = gb_sets:from_list(Nodes),

    VirtualNodes1 = lists:filter(fun ({_, N}) -> not gb_sets:is_member(N, NodeSet) end,
                                 lists:zip(tuple_to_list(VirtualNodeHashes0), tuple_to_list(VirtualNodes0))),

    Nodes1 = lists:filter(fun (N) -> not gb_sets:is_member(N, NodeSet) end, Nodes0),

    Ring#?RING{
            virtual_node_hashes = list_to_tuple([Hash || {Hash, _} <- VirtualNodes1]),
            virtual_nodes       = list_to_tuple([Node || {_, Node} <- VirtualNodes1]),
            nodes               = Nodes1,
            node_count          = length(Nodes1)
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
    #?RING{hash_mask = HashMask, node_count = NodeCount, virtual_nodes = VirtualNodes, virtual_node_hashes = VirtualNodeHashes} = Ring,
    ItemHash = hash(Item, Ring),
    PartitionSize = max(1, (HashMask + 1) div tuple_size(VirtualNodeHashes)),
    Position = find_start_position(ItemHash, PartitionSize, VirtualNodeHashes),
    fold_successor_nodes(NodeCount, Position, VirtualNodes, Fun, Initial).

%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec find_start_position(term(), pos_integer(), tuple()) -> non_neg_integer().
find_start_position(ItemHash, PartitionSize, VirtualNodeHashes) ->
    find_start_position(ItemHash, PartitionSize, VirtualNodeHashes, 1, (ItemHash div PartitionSize) + 1, tuple_size(VirtualNodeHashes) + 1).

-spec find_start_position(term(), pos_integer(), tuple(), pos_integer(), pos_integer(), pos_integer()) -> pos_integer().
find_start_position(_ItemHash, _PartitionSize, _VirtualNodeHashes, Position, _, Position) ->
    Position;
find_start_position(ItemHash, PartitionSize, VirtualNodeHashes, Start, Current0, End) ->
    Current  = min(max(Start, Current0), End - 1),
    NodeHash = element(Current, VirtualNodeHashes),
    case NodeHash of
        ItemHash -> Current;
        _        ->
            Delta = ItemHash - NodeHash,
            Next  = Current + (Delta div PartitionSize),
            case Delta > 0 of
                true  -> find_start_position(ItemHash, PartitionSize, VirtualNodeHashes, Current + 1, Next + 1, End);
                false -> find_start_position(ItemHash, PartitionSize, VirtualNodeHashes, Start, Next - 1, Current)
            end
    end.

-spec fold_successor_nodes(non_neg_integer(), non_neg_integer(), tuple(), hash_ring:fold_fun(), term()) -> term().
fold_successor_nodes(RestNodeCount, StartPosition, VirtualNodes, Fun, Initial) ->
    fold_successor_nodes(RestNodeCount, StartPosition, VirtualNodes, Fun, Initial, gb_sets:empty()).

-spec fold_successor_nodes(non_neg_integer(), non_neg_integer(), tuple(), hash_ring:fold_fun(), term(), gb_sets:set(hash_ring:ring_node())) -> term().
fold_successor_nodes(0, _, _, _, Acc, _) ->
    Acc;
fold_successor_nodes(RestNodeCount, Position, VirtualNodes, Fun, Acc, IteratedNodes) when Position >= tuple_size(VirtualNodes) ->
    fold_successor_nodes(RestNodeCount, 1, VirtualNodes, Fun, Acc, IteratedNodes);
fold_successor_nodes(RestNodeCount, Position, VirtualNodes, Fun, Acc, IteratedNodes) ->
    Node = element(Position, VirtualNodes),
    case gb_sets:is_member(Node, IteratedNodes) of
        true  -> fold_successor_nodes(RestNodeCount, Position + 1, VirtualNodes, Fun, Acc, IteratedNodes);
        false ->
            case Fun(Node, Acc) of
                {false, Acc2} -> Acc2;
                {true,  Acc2} -> fold_successor_nodes(RestNodeCount - 1, Position + 1, VirtualNodes, Fun, Acc2, gb_sets:add(Node, IteratedNodes))
            end
    end.

-spec hash(term(), ring()) -> non_neg_integer().
hash(X, Ring) ->
    #?RING{hash_mask = HashMask, hash_algorithm = HashAlgorithm} = Ring,
    hash_ring_util:calc_hash(HashAlgorithm, X) band HashMask.
