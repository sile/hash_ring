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
-export([hash_algorithms/0]).
-export([is_pos_integer/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec calc_hash(hash_ring:hash_algorithm(), term()) -> non_neg_integer().
calc_hash(crc32, Node) ->
    erlang:crc32(term_to_binary(Node));
calc_hash(phash2, Node) ->
    erlang:phash2(Node, 16#100000000);
calc_hash(HashAlgorithm, Node) ->
    crypto:bytes_to_integer(crypto:hash(HashAlgorithm, term_to_binary(Node))).

-spec hash_byte_size(hash_ring:hash_algorithm()) -> pos_integer().
hash_byte_size(crc32)  -> 4;
hash_byte_size(phash2) -> 4;
hash_byte_size(md5)    -> 16;
hash_byte_size(sha)    -> 20;
hash_byte_size(sha256) -> 32.

-spec hash_algorithms() -> [hash_ring:hash_algorithm()].
hash_algorithms() ->
    [crc32, phash2, md5, sha, sha256].

-spec is_pos_integer(pos_integer() | term()) -> boolean().
is_pos_integer(X) -> is_integer(X) andalso X > 0.
