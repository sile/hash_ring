%% @copyright 2013-2016 Takeru Ohta <phjgt308@gmail.com>
%%
-module(hash_ring_util_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Test Cases
%%----------------------------------------------------------------------------------------------------------------------
hash_algorithms_test() ->
    lists:foreach(
      fun (A) ->
              Hash = hash_ring_util:calc_hash(A, hash_ring_node:make(foo)),
              ?assert(is_integer(Hash)),
              ?assert(Hash >= 0),
              ?assert(Hash < (1 bsl hash_ring_util:hash_byte_size(A) * 8))
      end,
      hash_ring_util:hash_algorithms()).
