%% @copyright 2013-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Base module for `hash_ring' implementating modules
%% @private
%% @end
-module(hash_ring_base).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([make/1]).
-export([get_nodes/1]).
-export([get_non_phantom_node_count/1]).
-export([get_hash_mask/1]).
-export([hash/2]).
-export([calc_virtual_node_count/2]).
-export([add_node/2]).
-export([remove_node/2]).

-export_type([state/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(DEFAULT_VIRTUAL_NODE_COUNT, 256).
-define(DEFAULT_MAX_HASH_BYTE_SIZE, 4).
-define(DEFAULT_HASH_ALGORITHM, md5).

-define(STATE, ?MODULE).
-record(?STATE,
        {
          nodes = #{}        :: hash_ring:node_map(),
          phantom_count = 0  :: non_neg_integer(),
          virtual_node_count :: pos_integer(),
          hash_mask          :: pos_integer(),
          hash_algorithm     :: hash_ring:hash_algorithm()
        }).

-opaque state() :: #?STATE{}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec make(hash_ring:options()) -> state().
make(Options) ->
    HashAlgorithm    = proplists:get_value(hash_algorithm, Options, ?DEFAULT_HASH_ALGORITHM),
    VirtualNodeCount = proplists:get_value(virtual_node_count, Options, ?DEFAULT_VIRTUAL_NODE_COUNT),
    MaxHashByteSize0 = proplists:get_value(max_hash_byte_size, Options, ?DEFAULT_MAX_HASH_BYTE_SIZE),
    _ = lists:member(HashAlgorithm, hash_ring_util:hash_algorithms()) orelse error(badarg, [Options]),
    _ = hash_ring_util:is_pos_integer(VirtualNodeCount) orelse error(badarg, [Options]),
    _ = hash_ring_util:is_pos_integer(MaxHashByteSize0) orelse error(badarg, [Options]),

    MaxHashByteSize  = min(MaxHashByteSize0, hash_ring_util:hash_byte_size(HashAlgorithm)),
    HashMask = (1 bsl MaxHashByteSize * 8) - 1,

    #?STATE{
        virtual_node_count = VirtualNodeCount,
        hash_mask = HashMask,
        hash_algorithm = HashAlgorithm
       }.

-spec get_nodes(state()) -> hash_ring:node_map().
get_nodes(#?STATE{nodes = Nodes}) ->
    Nodes.

-spec get_non_phantom_node_count(state()) -> non_neg_integer().
get_non_phantom_node_count(#?STATE{nodes = Nodes, phantom_count = PhantomCount}) ->
    maps:size(Nodes) - PhantomCount.

-spec get_hash_mask(state()) -> pos_integer().
get_hash_mask(#?STATE{hash_mask = Mask}) ->
    Mask.

-spec hash(term(), state()) -> non_neg_integer().
hash(X, #?STATE{hash_mask = HashMask, hash_algorithm = HashAlgorithm}) ->
    hash_ring_util:calc_hash(HashAlgorithm, X) band HashMask.

-spec calc_virtual_node_count(hash_ring:ring_node(), state()) -> non_neg_integer().
calc_virtual_node_count(Node, #?STATE{virtual_node_count = BaseVirtualNodeCount}) ->
    round(BaseVirtualNodeCount * hash_ring_node:get_weight(Node)).

-spec add_node(hash_ring:ring_node(), state()) -> state().
add_node(Node, State) ->
    Key = hash_ring_node:get_key(Node),
    Nodes = maps:put(Key, Node, State#?STATE.nodes),
    PhantomScore = phantom_score({ok, Node}) - phantom_score(maps:find(Key, State#?STATE.nodes)),
    State#?STATE{nodes = Nodes, phantom_count = State#?STATE.phantom_count + PhantomScore}.

-spec remove_node(hash_ring_node:key(), state()) -> state().
remove_node(Key, State) ->
    Nodes = maps:remove(Key, State#?STATE.nodes),
    PhantomScore = phantom_score(maps:find(Key, State#?STATE.nodes)),
    State#?STATE{nodes = Nodes, phantom_count = State#?STATE.phantom_count - PhantomScore}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec phantom_score({ok, hash_ring:ring_node()} | error) -> 0 | 1.
phantom_score(error)      -> 0;
phantom_score({ok, Node}) ->
    case hash_ring_node:get_weight(Node) > 0 of
        true  -> 0;
        false -> 1
    end.
