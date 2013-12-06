%% @copyright 2013 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
-module(hash_ring_static).

-export([
         make/2
        ]).

-export_type([
              ring/0,
              option/0
             ]).

-define(RING, ?MODULE).

-record(?RING,
        {
          virtual_nodes :: tuple(),
          nodes         :: gb_set()
        }).

-opaque ring() :: #?RING{}.

-define(DEFAULT_VIRTUAL_NODE_COUNT_PER_HOST, 5).
-define(DEFAULT_HASH_ALGORITHM, md5).

-type option() :: {virtual_node_count_per_host, pos_integer()}
                | {hash_algorithm, atom()}. % TODO

-spec make([term()], [option()]) -> ring().
make(Nodes, Options) ->
    VirtualNodeCount = proplists:get_value(virtual_node_count_per_host, Options, ?DEFAULT_VIRTUAL_NODE_COUNT_PER_HOST),
    HashAlgorithm    = proplists:get_value(hash_algorithm, Options, ?DEFAULT_HASH_ALGORITHM),

    VirtualNodes1 = lists:append([[begin 
                                       VirtualNodeHash = crypto:bytes_to_integer(crypto:hash(HashAlgorithm, term_to_binary({I, Node}))),
                                       {VirtualNodeHash, Node}
                                   end || I <- lists:seq(1, VirtualNodeCount)] || Node <- Nodes]),
    VirtualNodes2 = lists:keysort(1, VirtualNodes1),
    VirtualNodes3 = list_to_tuple(VirtualNodes2),
    #?RING{
       virtual_nodes = VirtualNodes3,
       nodes         = gb_sets:from_list(Nodes)
      }.
