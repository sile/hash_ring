%% @copyright 2013-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Application Internal Utility Functions
%% @private
%% @end
-module(hash_ring_util).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([calc_hash/2]).
-export([hash_byte_size/1]).
-export([is_available_node/1]).
-export([calc_virtual_node_count/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec calc_hash(hash_ring:hash_algorithms(), term()) -> non_neg_integer().
calc_hash(crc32, Node) ->
    erlang:crc32(term_to_binary(Node));
calc_hash(phash2, Node) ->
    erlang:phash2(Node, 16#100000000);
calc_hash(HashAlgorithm, Node) ->
    crypto:bytes_to_integer(crypto:hash(HashAlgorithm, term_to_binary(Node))).

-spec hash_byte_size(hash_ring:hash_algorithms()) -> pos_integer().
hash_byte_size(crc32)  -> 4;
hash_byte_size(phash2) -> 4;
hash_byte_size(md5)    -> 16;
hash_byte_size(sha)    -> 20;
hash_byte_size(sha256) -> 32.

-spec is_available_node(hash_ring:ring_node()) -> boolean().
is_available_node(Node) ->
    hash_ring_node:get_weight(Node) > 0.

-spec calc_virtual_node_count(non_neg_integer(), hash_ring:ring_node()) -> VirtualNodeCount::non_neg_integer().
calc_virtual_node_count(BaseVirtualNodeCount, Node) ->
    round(BaseVirtualNodeCount * hash_ring_node:get_weight(Node)).
