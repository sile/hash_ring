%% @copyright 2013 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc hash_ringアプリケーション用のユーティリティ関数を集めたモジュール
%% @private
-module(hash_ring_util).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([
         calc_hash/2
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
