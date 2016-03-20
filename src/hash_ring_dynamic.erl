%% @copyright 2013-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Balanced Tree based `hash_ring' Implementation Module
%%
%% This module represents a ring (i.e., virtual nodes) as a [General Balanced Tree](http://erlang.org/doc/man/gb_trees.html).
%%
%% Dynamic addition and removal of nodes require O(log N) time.
%% It is superior to {@link hash_ring_dynamic} at the expense of node search efficiency and memory footprint.
%%
%% See [README.md](../README.md#benchmark) for a benchmark result.
%%
%% @end
-module(hash_ring_dynamic).

-behaviour(hash_ring).

%%----------------------------------------------------------------------------------------------------------------------
%% 'hash_ring' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([make/2, add_nodes/2, remove_nodes/2, get_nodes/1, fold/4]).

%%--------------------------------------------------------------------------------
%% Macros & Recors & Types
%%--------------------------------------------------------------------------------
-define(RING, ?MODULE).
-record(?RING,
        {
          vnodes :: vnodes(),
          base   :: hash_ring_base:state()
        }).

-type vnode_key() :: {Hash::non_neg_integer(), sequence(), hash_ring_node:key()}.
-type vnode_value() :: hash_ring:ring_node().
-type vnodes() :: gb_trees:tree(vnode_key(), vnode_value()).
-type sequence() :: non_neg_integer() | -1. % `-1' is the sentinel value

%%----------------------------------------------------------------------------------------------------------------------
%% 'hash_ring' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
make(Nodes, Options) ->
    Ring =
        #?RING{
            vnodes = gb_trees:empty(),
            base   = hash_ring_base:make(Options)
           },
    add_nodes(Nodes, Ring).

%% @private
add_nodes(Nodes, Ring) ->
    lists:foldl(fun add_node/2, Ring, Nodes).

%% @private
remove_nodes(Keys, Ring) ->
    lists:foldl(fun remove_node/2, Ring, Keys).

%% @private
get_nodes(Ring) ->
    hash_ring_base:get_nodes(Ring#?RING.base).

%% @private
fold(Fun, Item, Initial, Ring) ->
    #?RING{base = Base, vnodes = VirtualNodes} = Ring,
    NodeCount = hash_ring_base:get_non_phantom_node_count(Base),
    ItemHash = hash_ring_base:hash(Item, Base),
    Position = find_start_position(ItemHash, VirtualNodes),
    fold_successor_nodes(NodeCount, Position, VirtualNodes, Fun, Initial, gb_sets:empty()).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec find_start_position(term(), vnodes()) -> gb_trees:iter(vnode_key(), vnode_value()).
find_start_position(ItemHash, VirtualNodes) ->
    VnodeKey = {ItemHash, -1, undefined},
    gb_trees:iterator_from(VnodeKey, VirtualNodes).

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

-spec add_node(hash_ring:ring_node(), #?RING{}) -> #?RING{}.
add_node(Node, Ring) ->
    VirtualNodes = update_virtual_nodes(Node, Ring#?RING.vnodes, Ring#?RING.base),
    Base = hash_ring_base:add_node(Node, Ring#?RING.base),
    Ring#?RING{vnodes = VirtualNodes, base = Base}.

-spec remove_node(hash_ring_node:key(), #?RING{}) -> #?RING{}.
remove_node(Key, Ring) ->
    VirtualNodes = remove_virtual_nodes(Key, Ring#?RING.vnodes, 0, Ring#?RING.base),
    Base = hash_ring_base:remove_node(Key, Ring#?RING.base),
    Ring#?RING{vnodes = VirtualNodes, base = Base}.

-spec update_virtual_nodes(hash_ring:ring_node(), vnodes(), hash_ring_base:state()) -> vnodes().
update_virtual_nodes(Node, VirtualNodes, Base) ->
    ExistingNodes = hash_ring_base:get_nodes(Base),
    VirtualNodeCount = hash_ring_base:calc_virtual_node_count(Node, Base),
    case maps:find(hash_ring_node:get_key(Node), ExistingNodes) of
        error     -> add_virtual_nodes(Node, VirtualNodes, 0, VirtualNodeCount, Base);
        {ok, Old} ->
            OldCount = hash_ring_base:calc_virtual_node_count(Old, Base),
            case OldCount =< VirtualNodeCount of
                true  -> add_virtual_nodes(Node, VirtualNodes, OldCount, VirtualNodeCount, Base);
                false -> remove_virtual_nodes(hash_ring_node:get_key(Node), VirtualNodes, VirtualNodeCount, Base)
            end
    end.

-spec add_virtual_nodes(hash_ring:ring_node(), vnodes(), sequence(), sequence(), hash_ring_base:state()) -> vnodes().
add_virtual_nodes(Node, VirtualNodes, Start, End, Base) ->
    Key = hash_ring_node:get_key(Node),
    lists:foldl(
      fun (Seq, Acc) ->
              VnodeKey = {hash_ring_base:hash({Seq, Key}, Base), Seq, Key},
              gb_trees:insert(VnodeKey, Node, Acc)
      end,
      VirtualNodes,
      lists:seq(Start, End - 1)).

-spec remove_virtual_nodes(hash_ring_node:key(), vnodes(), sequence(), hash_ring_base:state()) -> vnodes().
remove_virtual_nodes(Key, VirtualNodes, Seq, Base) ->
    Hash = hash_ring_base:hash({Seq, Key}, Base),
    VnodeKey = {Hash, Seq, Key},
    case gb_trees:lookup(VnodeKey, VirtualNodes) of
        none       -> VirtualNodes;
        {value, _} -> remove_virtual_nodes(Key, gb_trees:delete(VnodeKey, VirtualNodes), Seq + 1, Base)
    end.
