%% @copyright 2013-2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc コンシステントハッシュリング操作用のインターフェースモジュール
-module(hash_ring).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([
         make/1,
         make/2,
         is_ring/1,
         add_nodes/2,
         remove_nodes/2,
         get_nodes/1,
         get_node_count/1,
         fold/4,
         find_node/2,
         collect_nodes/3
        ]).

-export_type([
              ring/0,
              ring_node/0,
              hash_ring_module/0,
              option/0,
              item/0,
              fold_fun/0,
              hash_algorithms/0
             ]).

%%--------------------------------------------------------------------------------
%% Behaviour API
%%--------------------------------------------------------------------------------
-callback make([ring_node()], [option()]) -> impl_state().
-callback is_ring(impl_state() | term()) -> boolean().
-callback add_nodes([ring_node()], impl_state()) -> impl_state().
-callback remove_nodes([ring_node()], impl_state()) -> impl_state().
-callback get_nodes(impl_state()) -> [ring_node()].
-callback get_node_count(impl_state()) -> non_neg_integer().
-callback fold(fold_fun(), item(), AccInitial::term(), impl_state()) -> AccResult::term().

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
-type ring_node() :: hash_ring_node:ring_node().

-type item() :: term().

-type option() :: {module, hash_ring_module()}
                | hash_ring_static:option().

-type hash_ring_module() :: hash_ring_static.

-type impl_state() :: term().

-type fold_fun() :: fun ((ring_node(), Acc::term()) -> {Continue::boolean(), AccNext::term()}).

-type hash_algorithms() :: crc32 | phash2 | md5 | sha | sha256.

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

%% @doc 引数の値が適切に生成されたリングオブジェクトかどうかを判定する
-spec is_ring(ring() | term()) -> boolean().
is_ring(#?RING{impl_module = M, impl_state = S}) -> M:is_ring(S);
is_ring(_)                                       -> false.

%% @doc ノード群を追加する
%%
%% キーが等しいノードが既に存在する場合は、上書きされる
-spec add_nodes([ring_node()], ring()) -> ring().
add_nodes(Nodes, Ring) ->
    #?RING{impl_module = Module, impl_state = State0} = Ring,
    State1 = Module:add_nodes(Nodes, State0),
    Ring#?RING{impl_state = State1}.

%% @doc ノード群を削除する
-spec remove_nodes([ring_node()], ring()) -> ring().
remove_nodes(Nodes, Ring) ->
    #?RING{impl_module = Module, impl_state = State0} = Ring,
    State1 = Module:remove_nodes(Nodes, State0),
    Ring#?RING{impl_state = State1}.

%% @doc ノード一覧を取得する
%%
%% 返り値のノードはキー順に昇順にソートされている
-spec get_nodes(ring()) -> [ring_node()].
get_nodes(Ring) ->
    #?RING{impl_module = Module, impl_state = State} = Ring,
    Module:get_nodes(State).

%% @doc ノードの個数を取得する
-spec get_node_count(ring()) -> non_neg_integer().
get_node_count(Ring) ->
    #?RING{impl_module = Module, impl_state = State} = Ring,
    Module:get_node_count(State).

%% @doc アイテムの次に位置するノードから順に畳み込みを行う
-spec fold(fold_fun(), item(), term(), ring()) -> Result::term().
fold(Fun, Item, Initial, Ring) ->
    #?RING{impl_module = Module, impl_state = State} = Ring,
    Module:fold(Fun, Item, Initial, State).

%% @doc 指定のアイテムを担当するノードを検索する
%%
%% リングが空の場合は`error'が返される
-spec find_node(item(), ring()) -> {ok, ring_node()} | error.
find_node(Item, Ring) ->
    fold(fun (Node, _) -> {false, {ok, Node}} end,
         Item,
         error,
         Ring).

%% @doc 指定のアイテムを担当するノードを優先順位が高い順に最大`N'個集める
-spec collect_nodes(item(), non_neg_integer(), ring()) -> [ring_node()].
collect_nodes(Item, N, Ring) ->
    {_, Nodes} =
        fold(fun (_Node, {Rest, Acc}) when Rest =< 0 ->
                     {false, {Rest, Acc}};
                 (Node, {Rest, Acc}) ->
                     {Rest > 1, {Rest - 1, [Node | Acc]}}
             end,
             Item,
             {N, []},
             Ring),
    lists:reverse(Nodes).
