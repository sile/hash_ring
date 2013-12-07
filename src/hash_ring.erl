%% @copyright 2013 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc コンシステントハッシュリング操作用のインターフェースモジュール
-module(hash_ring).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([
         make/1,
         make/2,
         get_nodes/1,
         get_owner_nodes/3
        ]).

-export_type([
              ring/0,
              ring_node/0,
              hash_ring_module/0,
              option/0,
              item/0
             ]).

%%--------------------------------------------------------------------------------
%% Behaviour API
%%--------------------------------------------------------------------------------
-callback make([ring_node()], [option()]) -> impl_state().
-callback get_nodes(impl_state()) -> [ring_node()].
-callback get_owner_nodes(item(), MaxOwnerCount::non_neg_integer(), impl_state()) -> [ring_node()].

%%--------------------------------------------------------------------------------
%% Macros & Records & Types
%%--------------------------------------------------------------------------------
-define(RING, ?MODULE).
-define(DEFAULT_HASH_RING_MODULE, hash_ring_static).

-record(?RING,
        {
          impl_module :: module(),
          impl_state  :: impl_state()
        }).

-opaque ring()    :: #?RING{}.
-type ring_node() :: term().

-type item() :: term().

-type option() :: {module, hash_ring_module()}
                | hash_ring_static:option().

-type hash_ring_module() :: hash_ring_static.

-type impl_state() :: term().

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
%% @doc equiv make(Nodes, [])
-spec make([ring_node()]) -> ring().
make(Nodes) ->
    make(Nodes, []).

%% @doc コンシステントハッシュリングを構築する
-spec make([ring_node()], [option()]) -> ring().
make(Nodes, Options) ->
    Module = proplists:get_value(module, Options, ?DEFAULT_HASH_RING_MODULE),
    State = Module:make(Nodes, Options),
    #?RING{impl_module = Module, impl_state = State}.

%% @doc ノード一覧を取得する
-spec get_nodes(ring()) -> [ring_node()].
get_nodes(Ring) ->
    #?RING{impl_module = Module, impl_state = State} = Ring,
    Module:get_nodes(State).

%% @doc アイテムの所有者となるノードを優先度が高い順に返す
-spec get_owner_nodes(item(), non_neg_integer(), ring()) -> [ring_node()].
get_owner_nodes(Item, MaxOwnerCount, Ring) ->
    #?RING{impl_module = Module, impl_state = State} = Ring,
    Module:get_owner_nodes(Item, MaxOwnerCount, State).
