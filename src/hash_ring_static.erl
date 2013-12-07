%% @copyright 2013 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc 静的に構築されたコンシステントハッシュリングを操作するためのモジュール
-module(hash_ring_static).

-behaviour(hash_ring).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([
         make/2,
         get_nodes/1,
         get_owner_nodes/3
        ]).

-export_type([
              ring/0,
              option/0
             ]).

%%--------------------------------------------------------------------------------
%% Macros & Recors & Types
%%--------------------------------------------------------------------------------
-define(RING, ?MODULE).
-define(DEFAULT_VIRTUAL_NODE_COUNT_PER_HOST, 5).
-define(DEFAULT_HASH_ALGORITHM, md5).

-record(?RING,
        {
          virtual_nodes  :: tuple(),
          nodes          :: [hash_ring:ring_node()],
          hash_algorithm :: crypto:hash_algorithms()
        }).

-opaque ring() :: #?RING{}.

-type option() :: {virtual_node_count_per_host, pos_integer()}
                | {hash_algorithm, crypto:hash_algorithms()}.

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
%% @doc コンシステントハッシュリングを構築する
-spec make([hash_ring:ring_node()], [option()]) -> ring().
make(Nodes, Options) ->
    VirtualNodeCount = proplists:get_value(virtual_node_count_per_host, Options, ?DEFAULT_VIRTUAL_NODE_COUNT_PER_HOST),
    HashAlgorithm    = proplists:get_value(hash_algorithm, Options, ?DEFAULT_HASH_ALGORITHM),

    VirtualNodes1 = lists:append([[begin 
                                       VirtualNodeHash = hash_ring_util:calc_hash(HashAlgorithm, {I, Node}),
                                       {VirtualNodeHash, Node}
                                   end || I <- lists:seq(1, VirtualNodeCount)] || Node <- Nodes]),
    VirtualNodes2 = lists:sort(VirtualNodes1), % lists:keysort/2 だとハッシュ値に衝突がある場合に、順番が一意に定まらないので単なる sort/1 を使用する
    VirtualNodes3 = list_to_tuple(VirtualNodes2),
    #?RING{
        virtual_nodes  = VirtualNodes3,
        nodes          = Nodes,
        hash_algorithm = HashAlgorithm
       }.

%% @doc ノード一覧を取得する
-spec get_nodes(ring()) -> [hash_ring:ring_node()].
get_nodes(Ring) ->
    Ring#?RING.nodes.

%% @doc アイテムの所有者となるノードを優先度が高い順に返す
-spec get_owner_nodes(hash_ring:item(), non_neg_integer(), ring()) -> [hash_ring:ring_node()].
get_owner_nodes(Item, MaxOwnerCount, Ring) ->
    #?RING{hash_algorithm = HashAlgorithm, virtual_nodes = VirtualNodes} = Ring,
    ItemHash = hash_ring_util:calc_hash(HashAlgorithm, Item),
    Position = find_start_position(ItemHash, VirtualNodes),
    collect_successor_nodes(Position, MaxOwnerCount, VirtualNodes).

%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec find_start_position(term(), tuple()) -> non_neg_integer().
find_start_position(ItemHash, VirtualNodes) ->
    find_start_position(ItemHash, VirtualNodes, 1, tuple_size(VirtualNodes) + 1).

-spec find_start_position(term(), tuple(), non_neg_integer(), non_neg_integer()) -> non_neg_integer().
find_start_position(_ItemHash, _VirtualNodes, Pos, Pos) ->
    Pos;
find_start_position(ItemHash, VirtualNodes, Start, End) ->
    Current = (Start + End) div 2,
    {NodeHash, _} = element(Current, VirtualNodes),
    if
        NodeHash < ItemHash -> find_start_position(ItemHash, VirtualNodes, Current + 1, End);
        NodeHash > ItemHash -> find_start_position(ItemHash, VirtualNodes, Start, Current);
        true                -> Current
    end.

-spec collect_successor_nodes(non_neg_integer(), non_neg_integer(), tuple()) -> [hash_ring:ring_node()].
collect_successor_nodes(StartPosition, Count, VirtualNodes) ->
    collect_successor_nodes(tuple_size(VirtualNodes), Count, StartPosition, VirtualNodes, gb_sets:empty(), []).

-spec collect_successor_nodes(non_neg_integer(), non_neg_integer(), non_neg_integer(), tuple(), gb_set(), [hash_ring:ring_node()]) -> [hash_ring:ring_node()].
collect_successor_nodes(0, _, _, _, _, AccNodes) ->
    lists:reverse(AccNodes);
collect_successor_nodes(_, 0, _, _, _, AccNodes) ->
    lists:reverse(AccNodes);
collect_successor_nodes(RestNodes, RestCount, Position, VirtualNodes, NodeSet, AccNodes) when Position > tuple_size(VirtualNodes) ->
    collect_successor_nodes(RestNodes, RestCount, 1, VirtualNodes, NodeSet, AccNodes);
collect_successor_nodes(RestNodes, RestCount, Position, VirtualNodes, NodeSet, AccNodes) ->
    {_, Node} = element(Position, VirtualNodes),
    {NodeSet2, AccNodes2, RestCount2} =
        case gb_sets:is_member(Node, NodeSet) of
            true  -> {NodeSet, AccNodes, RestCount};
            false -> {gb_sets:insert(Node, NodeSet), [Node | AccNodes], RestCount - 1}
        end,
    collect_successor_nodes(RestNodes - 1, RestCount2, Position + 1, VirtualNodes, NodeSet2, AccNodes2).
