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
-spec calc_hash(crypto:hash_algorithms(), hash_ring:ring_node()) -> non_neg_integer().
calc_hash(HashAlgorithm, Node) ->
    crypto:bytes_to_integer(crypto:hash(HashAlgorithm, term_to_binary(Node))).
