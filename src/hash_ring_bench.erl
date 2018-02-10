%% @copyright 2013-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Simple Benchmark Module
%% @private
%% @end
-module(hash_ring_bench).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([bench/2]).
-export([make_time/2]).
-export([add_time/2]).
-export([remove_time/2]).
-export([find_time/2]).
-export([heap_size/2]).
-export([external_size/2]).

-export_type([elapsed_time/0]).
-export_type([bytes/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type elapsed_time() :: MicroSeconds::non_neg_integer().
-type bytes() :: non_neg_integer().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec bench(non_neg_integer(), hash_ring:options()) -> [{Module, [Result]}] when
      Module :: hash_ring_static | hash_ring_dynamic,
      Result :: {make_time, elapsed_time()}
              | {add_time, elapsed_time()}
              | {remove_time, elapsed_time()}
              | {find_time, elapsed_time()}
              | {heap_size, bytes()}
              | {external_size, bytes()}.
bench(NodeCount, Options0) ->
    Modules = [hash_ring_static, hash_ring_dynamic],
    Nodes = hash_ring:list_to_nodes(lists:seq(1, NodeCount)),
    [begin
         Options1 = [{module, Module} | Options0],
         {Module,
          [
           {make_time, make_time(Nodes, Options1)},
           {add_time, add_time(Nodes, Options1)},
           {remove_time, remove_time(Nodes, Options1)},
           {find_time, find_time(Nodes, Options1)},
           {heap_size, heap_size(Nodes, Options1)},
           {external_size, external_size(Nodes, Options1)}
          ]}
     end || Module <- Modules].

-spec make_time([hash_ring:ring_node()], hash_ring:options()) -> elapsed_time().
make_time(Nodes, Options) ->
    erlang:garbage_collect(),
    {Elapsed, _} =
        timer:tc(fun () -> hash_ring:make(Nodes, Options) end),
    Elapsed.

-spec add_time([hash_ring:ring_node()], hash_ring:options()) -> elapsed_time().
add_time(Nodes, Options) ->
    Ring = hash_ring:make([], Options) ,
    erlang:garbage_collect(),
    {Elapsed, _} =
        timer:tc(fun () -> lists:foldl(fun hash_ring:add_node/2, Ring, Nodes) end),
    Elapsed.

-spec remove_time([hash_ring:ring_node()], hash_ring:options()) -> elapsed_time().
remove_time(Nodes, Options) ->
    Ring = hash_ring:make(Nodes, Options) ,
    Keys = lists:map(fun hash_ring_node:get_key/1, Nodes),
    erlang:garbage_collect(),
    {Elapsed, _} =
        timer:tc(fun () -> lists:foldl(fun hash_ring:remove_node/2, Ring, Keys) end),
    Elapsed.

-spec find_time([hash_ring:ring_node()], hash_ring:options()) -> elapsed_time().
find_time(Nodes, Options) ->
    Ring = hash_ring:make(Nodes, Options) ,
    Keys = lists:map(fun hash_ring_node:get_key/1, Nodes),
    erlang:garbage_collect(),
    {Elapsed, _} =
        timer:tc(
          fun () ->
                  lists:foreach(
                    fun (Key) -> hash_ring:find_node(Key, Ring) end,
                    Keys)
          end),
    Elapsed.

-spec heap_size([hash_ring:ring_node()], hash_ring:options()) -> bytes().
heap_size(Nodes, Options) ->
    Parent = self(),
    Tag = make_ref(),
    Ring = hash_ring:make(Nodes, Options),
    _ = spawn_opt(
          fun () ->
                  Count = hash_ring:get_node_count(Ring),
                  {_, HeapSize0} = erlang:process_info(self(), total_heap_size),
                  erlang:garbage_collect(),
                  {_, HeapSize1} = erlang:process_info(self(), total_heap_size),
                  HeapSize = HeapSize0 - HeapSize1,
                  Parent ! {Tag, HeapSize, Count}
          end,
          [link, {min_heap_size, 0}, {min_bin_vheap_size, 0}, {fullsweep_after, 0}]),
    receive
        {Tag, HeapSize, _} -> HeapSize
    end.

-spec external_size([hash_ring:ring_node()], hash_ring:options()) -> bytes().
external_size(Nodes, Options) ->
    erlang:external_size(hash_ring:make(Nodes, Options)).
