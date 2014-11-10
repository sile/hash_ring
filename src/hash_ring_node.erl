%% @copyright 2013-2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc コンシステントハッシュリング上のノードを表現するオブジェクト
-module(hash_ring_node).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([make/1, make/2, make/3]).
-export([get_key/1, get_data/1, get_weight/1]).

-export_type([ring_node/0]).
-export_type([key/0, data/0, weight/0]).
-export_type([option/0, options/0]).

%%--------------------------------------------------------------------------------
%% Macros & Records & Types
%%--------------------------------------------------------------------------------
-define(NODE, ?MODULE).

-record(?NODE,
        {
          key    :: key(),
          data   :: data(),
          weight :: weight()
        }).

-opaque ring_node() :: #?NODE{}.

-type key() :: term().
-type data() :: term().
-type weight() :: number(). % non negative

-type options() :: [option()].
-type option() :: {weight, weight()}.
%% weight: <br />
%%  - 仮想ノードの個数を決定する際の重み <br />
%%  - 値が大きいほど仮想ノードの個数が多くなり、より選択されやすくなる <br />
%%  - デフォルト値: `1.0' <br />

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
%% @equiv make(Key, Key)
-spec make(key()) -> ring_node().
make(Key) ->
    make(Key, Key).

%% @equiv make(Key, Data, [])
-spec make(key(), data()) -> ring_node().
make(Key, Data) ->
    make(Key, Data, []).

%% @doc `ring_node()'のインスタンスを生成する
-spec make(key(), data(), options()) -> ring_node().
make(Key, Data, Options) ->
    Weight = proplists:get_value(weight, Options, 1),
    _ = (is_number(Weight) andalso Weight >= 0) orelse error(badarg, [Key, Data, Options]),

    #?NODE{
        key    = Key,
        data   = Data,
        weight = Weight
       }.

%% @doc ノードのキーを取得する
-spec get_key(ring_node()) -> key().
get_key(#?NODE{key = Key}) -> Key.

%% @doc ノードのデータを取得する
-spec get_data(ring_node()) -> data().
get_data(#?NODE{data = Data}) -> Data.

%% @doc ノードの重みを取得する
-spec get_weight(ring_node()) -> weight().
get_weight(#?NODE{weight = Weight}) -> Weight.
