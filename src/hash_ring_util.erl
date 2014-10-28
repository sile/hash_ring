%% @copyright 2013 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc hash_ringアプリケーション用のユーティリティ関数を集めたモジュール
%% @private
-module(hash_ring_util).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([
         calc_hash/2,
         hash_byte_size/1,
         identity/1
        ]).

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
-spec calc_hash(hash_ring:hash_algorithms(), hash_ring:ring_node()) -> non_neg_integer().
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

-spec identity(term()) -> term().
identity(X) -> X.
