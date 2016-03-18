%% @copyright 2013-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc 静的に構築されたコンシステントハッシュリングを操作するためのモジュール
%% @end
-module(hash_ring_static).

-behaviour(hash_ring).

%%----------------------------------------------------------------------------------------------------------------------
%% 'hash_ring' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([make/2, add_nodes/2, remove_nodes/2, get_nodes/1, fold/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Recors & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(SENTINEL_NODE, hash_ring_node:make('SENTINEL')).

-define(RING, ?MODULE).
-record(?RING,
        {
          vnodes :: tuple(), % array of `virtual_node()'
          base   :: hash_ring_base:state()
        }).

-type virtual_node() :: {Hash::non_neg_integer(), Sequence::non_neg_integer(), hash_ring:ring_node()}.

%%----------------------------------------------------------------------------------------------------------------------
%% 'hash_ring' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
make(Nodes, Options) ->
    Base = hash_ring_base:make(Options),
    HashMask = hash_ring_base:get_hash_mask(Base),
    Ring =
        #?RING{
            vnodes = {{HashMask + 1, 0, ?SENTINEL_NODE}},
            base   = Base
           },
    add_nodes(Nodes, Ring).

%% @private
add_nodes(Nodes, Ring) ->
    UniqueNodes =
        maps:values(
          lists:foldl(
            fun (N, Acc) -> maps:put(hash_ring_node:get_key(N), N, Acc) end,
            #{},
            Nodes)),
    VirtualNodes =
        lists:sort(
          lists:foldl(fun (Node, Acc) -> add_node(Node, Acc, Ring#?RING.base) end,
                      tuple_to_list(Ring#?RING.vnodes),
                      UniqueNodes)),
    Base =
        lists:foldl(fun hash_ring_base:add_node/2, Ring#?RING.base, UniqueNodes),
    Ring#?RING{
            vnodes = list_to_tuple(VirtualNodes),
            base   = Base
           }.

%% @private
remove_nodes(Keys, Ring) ->
    KeySet =
        gb_sets:from_list(Keys),
    VirtualNodes =
        lists:filter(fun ({_, _, N}) -> not gb_sets:is_member(hash_ring_node:get_key(N), KeySet) end,
                     tuple_to_list(Ring#?RING.vnodes)),
    Base =
        gb_sets:fold(fun hash_ring_base:remove_node/2, Ring#?RING.base, KeySet),
    Ring#?RING{
            vnodes = list_to_tuple(VirtualNodes),
            base   = Base
           }.

%% @private
get_nodes(Ring) ->
    hash_ring_base:get_nodes(Ring#?RING.base).

%% @private
fold(Fun, Item, Initial, Ring) ->
    #?RING{base = Base, vnodes = VirtualNodes} = Ring,
    HashMask = hash_ring_base:get_hash_mask(Base),
    ItemHash = hash_ring_base:hash(Item, Base),
    NodeCount = hash_ring_base:get_non_phantom_node_count(Base),
    PartitionSize = max(1, (HashMask + 1) div tuple_size(VirtualNodes)),
    Position = find_start_position(ItemHash, PartitionSize, VirtualNodes),
    fold_successor_nodes(NodeCount, Position, VirtualNodes, Fun, Initial).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
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

-spec fold_successor_nodes(non_neg_integer(), non_neg_integer(), tuple(), hash_ring:fold_fun(), term(),
                           gb_sets:set(hash_ring:ring_node())) -> term().
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
                {true,  Acc2} -> fold_successor_nodes(RestNodeCount - 1, Position + 1, VirtualNodes,
                                                      Fun, Acc2, gb_sets:add(Node, IteratedNodes))
            end
    end.

-spec add_node(hash_ring:ring_node(), [virtual_node()], hash_ring_base:state()) -> [virtual_node()].
add_node(Node, VirtualNodes, Base) ->
    ExistingNodes = hash_ring_base:get_nodes(Base),
    VirtualNodeCount = hash_ring_base:calc_virtual_node_count(Node, Base),
    case maps:find(hash_ring_node:get_key(Node), ExistingNodes) of
        error     -> add_virtual_nodes(Node, VirtualNodes, 0, VirtualNodeCount, Base);
        {ok, Old} ->
            OldCount = hash_ring_base:calc_virtual_node_count(Old, Base),
            case OldCount =< VirtualNodeCount of
                true  -> add_virtual_nodes(Node, VirtualNodes, OldCount, VirtualNodeCount, Base);
                false -> remove_virtual_nodes(Node, VirtualNodes, VirtualNodeCount)
            end
    end.

-spec add_virtual_nodes(hash_ring:ring_node(), [virtual_node()], non_neg_integer(), non_neg_integer(),
                        hash_ring_base:state()) -> [virtual_node()].
add_virtual_nodes(Node, VirtualNodes, Start, End, Base) ->
    Key = hash_ring_node:get_key(Node),
    [{hash_ring_base:hash({Seq, Key}, Base), Seq, Node} || Seq <- lists:seq(Start, End - 1)] ++ VirtualNodes.

-spec remove_virtual_nodes(hash_ring:ring_node(), [virtual_node()], non_neg_integer()) -> [virtual_node()].
remove_virtual_nodes(Node, VirtualNodes, End) ->
    Key = hash_ring_node:get_key(Node),
    lists:filter(fun ({_, Seq, N}) -> Seq < End orelse hash_ring_node:get_key(N) =/= Key end,
                 VirtualNodes).
