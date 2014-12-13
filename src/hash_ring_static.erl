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
          virtual_node_count :: pos_integer(),
          vnodes             :: tuple(), % array of `virtual_node()'
          nodes              :: [hash_ring:ring_node()],
          node_count         :: non_neg_integer(),
          hash_mask          :: integer(),
          hash_algorithm     :: hash_ring:hash_algorithms(),
          weight_mode        :: weight_mode
        }).

-opaque ring() :: #?RING{}.

-type option() :: {virtual_node_count, pos_integer()}
                | {max_hash_byte_size, pos_integer()}
                | {hash_algorithm, hash_ring:hash_algorithms()}
                | {weight_mode, weight_mode()} % default: direct
                | {sentinel_key, hash_ring_node:key()}. % for unit-test

-type virtual_node() :: {Hash::non_neg_integer(), Sequence::non_neg_integer(), hash_ring:ring_node()}.

-type weight_mode() :: direct | average.
%% - direct: 指定された重みをそのまま使う
%% - average: 各ノードの重みを平均化する

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
%% @doc コンシステントハッシュリングを構築する
-spec make([hash_ring:ring_node()], [option()]) -> ring().
make(Nodes, Options) ->
    SentinelKey      = proplists:get_value(sentinel_key, Options, make_ref()),
    VirtualNodeCount = proplists:get_value(virtual_node_count, Options, ?DEFAULT_VIRTUAL_NODE_COUNT),
    HashAlgorithm    = proplists:get_value(hash_algorithm, Options, ?DEFAULT_HASH_ALGORITHM),
    WeightMode       = proplists:get_value(weight_mode, Options, ?DEFAULT_WEIGHT_MODE),
    MaxHashByteSize0 = proplists:get_value(max_hash_byte_size, Options, ?DEFAULT_MAX_HASH_BYTE_SIZE),
    MaxHashByteSize  = min(MaxHashByteSize0, hash_ring_util:hash_byte_size(HashAlgorithm)),

    HashMask = (1 bsl MaxHashByteSize * 8) - 1,

    Ring =
        #?RING{
            virtual_node_count = VirtualNodeCount,
            vnodes             = {{HashMask + 1, 0, hash_ring_node:make(SentinelKey)}}, % store sentinel
            nodes              = [],
            node_count         = 0,
            hash_mask          = HashMask,
            hash_algorithm     = HashAlgorithm,
            weight_mode        = WeightMode
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
                    tuple_to_list(VirtualNodes0),
                    UniqNodes),
    VirtualNodes2 = lists:sort(VirtualNodes1), % lists:keysort/2 だとハッシュ値に衝突がある場合に、順番が一意に定まらないので単なる sort/1 を使用する

    Nodes1 = lists:umerge(fun node_less_than_or_equal_to/2, UniqNodes, Nodes0),

    Ring#?RING{
            vnodes     = list_to_tuple(VirtualNodes2),
            nodes      = Nodes1,
            node_count = length(Nodes1)
           }.

%% @doc ノード群を削除する
-spec remove_nodes([hash_ring_node:key()], ring()) -> ring().
remove_nodes(Keys, Ring) ->
    #?RING{vnodes = VirtualNodes0, nodes = Nodes0} = Ring,
    KeySet = gb_sets:from_list(Keys),

    VirtualNodes1 =
        lists:filter(fun ({_, _, N}) -> not gb_sets:is_member(hash_ring_node:get_key(N), KeySet) end,
                     tuple_to_list(VirtualNodes0)),
    Nodes1 =
        lists:filter(fun (N) -> not gb_sets:is_member(hash_ring_node:get_key(N), KeySet) end, Nodes0),

    Ring#?RING{
            vnodes     = list_to_tuple(VirtualNodes1),
            nodes      = Nodes1,
            node_count = length(Nodes1)
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
fold(_, _, Initial, #?RING{vnodes = {_}}) ->
    Initial;
fold(Fun, Item, Initial, Ring) ->
    #?RING{hash_mask = HashMask, node_count = NodeCount, vnodes = VirtualNodes} = Ring,
    ItemHash = hash(Item, Ring),
    PartitionSize = max(1, (HashMask + 1) div tuple_size(VirtualNodes)),
    Position = find_start_position(ItemHash, PartitionSize, VirtualNodes),
    fold_successor_nodes(NodeCount, Position, VirtualNodes, Fun, Initial).

%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec find_start_position(term(), pos_integer(), tuple()) -> non_neg_integer().
find_start_position(ItemHash, PartitionSize, VirtualNodes) ->
    find_start_position(ItemHash, PartitionSize, VirtualNodes, 1, (ItemHash div PartitionSize) + 1, tuple_size(VirtualNodes) + 1).

-spec find_start_position(term(), pos_integer(), tuple(), pos_integer(), pos_integer(), pos_integer()) -> pos_integer().
find_start_position(_ItemHash, _PartitionSize, _VirtualNodes, Position, _, Position) ->
    Position;
find_start_position(ItemHash, PartitionSize, VirtualNodes, Start, Current0, End) ->
    Current = min(max(Start, Current0), End - 1),
    {NodeHash, _, _} = element(Current, VirtualNodes),
    case NodeHash of
        ItemHash -> Current;
        _        ->
            Delta = ItemHash - NodeHash,
            Next  = Current + (Delta div PartitionSize),
            case Delta > 0 of
                true  -> find_start_position(ItemHash, PartitionSize, VirtualNodes, Current + 1, Next + 1, End);
                false -> find_start_position(ItemHash, PartitionSize, VirtualNodes, Start, Next - 1, Current)
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
    {_, _, Node} = element(Position, VirtualNodes),
    case gb_sets:is_member(Node, IteratedNodes) of
        true  -> fold_successor_nodes(RestNodeCount, Position + 1, VirtualNodes, Fun, Acc, IteratedNodes);
        false ->
            case Fun(Node, Acc) of
                {false, Acc2} -> Acc2;
                {true,  Acc2} -> fold_successor_nodes(RestNodeCount - 1, Position + 1, VirtualNodes, Fun, Acc2, gb_sets:add(Node, IteratedNodes))
            end
    end.

-spec add_node(hash_ring:ring_node(), [virtual_node()], ring()) -> [virtual_node()].
add_node(Node, VirtualNodes, Ring) ->
    #?RING{nodes = ExistingNodes, virtual_node_count = BaseCount} = Ring,
    VirtualNodeCount = hash_ring_node:calc_virtual_node_count(BaseCount, Node),
    case node_find(hash_ring_node:get_key(Node), ExistingNodes) of
        error     -> add_virtual_nodes(Node, VirtualNodes, 0, VirtualNodeCount, Ring);
        {ok, Old} ->
            OldCount = hash_ring_node:calc_virtual_node_count(BaseCount, Old),
            case OldCount =< VirtualNodeCount of
                true  -> add_virtual_nodes(Node, VirtualNodes, OldCount, VirtualNodeCount, Ring);
                false -> remove_virtual_nodes(Node, VirtualNodes, VirtualNodeCount)
            end
    end.

-spec add_virtual_nodes(hash_ring:ring_node(), [virtual_node()], non_neg_integer(), non_neg_integer(), ring()) -> [virtual_node()].
add_virtual_nodes(Node, VirtualNodes, Start, End, Ring) ->
    Key = hash_ring_node:get_key(Node),
    [{hash({Seq, Key}, Ring), Seq, Node} || Seq <- lists:seq(Start, End - 1)] ++ VirtualNodes.

-spec remove_virtual_nodes(hash_ring:ring_node(), [virtual_node()], non_neg_integer()) -> [virtual_node()].
remove_virtual_nodes(Node, VirtualNodes, End) ->
    Key = hash_ring_node:get_key(Node),
    lists:filter(fun ({_, Seq, N}) -> Seq < End orelse hash_ring_node:get_key(N) =/= Key end,
                 VirtualNodes).

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
