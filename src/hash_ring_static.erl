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
-define(DEFAULT_VIRTUAL_NODE_COUNT, 128). % 各ノードごとの仮想ノードの数
-define(DEFAULT_HASH_ALGORITHM, md5).

-record(?RING,
        {
          virtual_nodes  :: tuple(),
          nodes          :: [hash_ring:ring_node()],
          hash_algorithm :: hash_ring:hash_algorithms()
        }).

-opaque ring() :: #?RING{}.

-type option() :: {virtual_node_count, pos_integer()}
                | {hash_algorithm, hash_ring:hash_algorithms()}.

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
%% @doc コンシステントハッシュリングを構築する
-spec make([hash_ring:ring_node()], [option()]) -> ring().
make(Nodes, Options) ->
    VirtualNodeCount = proplists:get_value(virtual_node_count, Options, ?DEFAULT_VIRTUAL_NODE_COUNT),
    HashAlgorithm    = proplists:get_value(hash_algorithm, Options, ?DEFAULT_HASH_ALGORITHM),

    VirtualNodes1 = lists:append([[begin 
                                       VirtualNodeHash = hash_ring_util:calc_hash(HashAlgorithm, {I, Node}),
                                       {VirtualNodeHash, Node}
                                   end || I <- lists:seq(1, VirtualNodeCount)] || Node <- Nodes]),
    VirtualNodes2 = lists:sort(VirtualNodes1), % lists:keysort/2 だとハッシュ値に衝突がある場合に、順番が一意に定まらないので単なる sort/1 を使用する
    VirtualNodes3 = list_to_tuple(VirtualNodes2),
    #?RING{
        virtual_nodes  = VirtualNodes3,
        nodes          = lists:usort(Nodes),
        hash_algorithm = HashAlgorithm
       }.

%% @doc ノード一覧を取得する
-spec get_nodes(ring()) -> [hash_ring:ring_node()].
get_nodes(Ring) ->
    Ring#?RING.nodes.

%% @doc アイテムの次に位置するノードから順に畳み込みを行う
-spec fold(hash_ring:fold_fun(), hash_ring:item(), term(), ring()) -> Result::term().
fold(Fun, Item, Initial, Ring) ->
    #?RING{hash_algorithm = HashAlgorithm, virtual_nodes = VirtualNodes, nodes = Nodes} = Ring,
    ItemHash = hash_ring_util:calc_hash(HashAlgorithm, Item),
    Position = find_start_position(ItemHash, VirtualNodes),
    fold_successor_nodes(length(Nodes), Position, VirtualNodes, Fun, Initial).

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

-spec fold_successor_nodes(non_neg_integer(), non_neg_integer(), tuple(), hash_ring:fold_fun(), term()) -> term().
fold_successor_nodes(RestNodeCount, StartPosition, VirtualNodes, Fun, Initial) ->
    fold_successor_nodes(RestNodeCount, StartPosition, VirtualNodes, Fun, Initial, []).

-spec fold_successor_nodes(non_neg_integer(), non_neg_integer(), tuple(), hash_ring:fold_fun(), term(), [hash_ring:ring_node()]) -> term().
fold_successor_nodes(0, _, _, _, Acc, _) ->
    Acc;
fold_successor_nodes(RestNodeCount, Position, VirtualNodes, Fun, Acc, IteratedNodes) when Position > tuple_size(VirtualNodes) ->
    fold_successor_nodes(RestNodeCount, 1, VirtualNodes, Fun, Acc, IteratedNodes);
fold_successor_nodes(RestNodeCount, Position, VirtualNodes, Fun, Acc, IteratedNodes) ->
    {_, Node} = element(Position, VirtualNodes),
    case lists:member(Node, IteratedNodes) of % NOTE: ノード数が多くなるとスケールしない
        true  -> fold_successor_nodes(RestNodeCount, Position + 1, VirtualNodes, Fun, Acc, IteratedNodes);
        false ->
            case Fun(Node, Acc) of
                {false, Acc2} -> Acc2;
                {true,  Acc2} -> fold_successor_nodes(RestNodeCount - 1, Position + 1, VirtualNodes, Fun, Acc2, [Node | IteratedNodes])
            end
    end.
