%% @copyright 2013-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc An object which represents a node on a consistent hash ring
%% @end
-module(hash_ring_node).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([make/1, make/2, make/3]).
-export([is_node/1]).
-export([get_key/1, get_data/1, get_weight/1]).

-export_type([ring_node/0]).
-export_type([key/0, data/0, weight/0]).
-export_type([option/0, options/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(NODE, ?MODULE).
-record(?NODE,
        {
          key    :: key(),
          data   :: data(),
          weight :: weight()
        }).

-opaque ring_node() :: #?NODE{}.
%% A node on a ring.

-type key() :: term().
%% The key of a `ring_node()'.
%%
%% It is used to decide location of the node on a ring.

-type data() :: term().
%% The data of a `ring_node()'.
%%
%% It holds arbitrary user data.

-type weight() :: number().
%% The non negative weight of a `ring_node()'.
%%
%% The more weight node occupies, the more space in a ring.

-type options() :: [option()].
-type option() :: {weight, weight()}.
%% weight:
%% <ul>
%% <li>A coefficient which is used to determine the virtual node count of the node.</li>
%% <li>The higher the value, the number of virtual nodes increases, likely to be more selected.</li>
%% <li>The default value is `1'.</li>
%% </ul>

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv make(Key, Key)
-spec make(key()) -> ring_node().
make(Key) ->
    make(Key, Key).

%% @equiv make(Key, Data, [])
-spec make(key(), data()) -> ring_node().
make(Key, Data) ->
    make(Key, Data, []).

%% @doc Creates a new `ring_node()' object
-spec make(key(), data(), options()) -> ring_node().
make(Key, Data, Options) ->
    Weight = proplists:get_value(weight, Options, 1),
    _ = (is_number(Weight) andalso Weight >= 0) orelse error(badarg, [Key, Data, Options]),

    #?NODE{
        key    = Key,
        data   = Data,
        weight = Weight
       }.

%% @doc Returns `true' if `X' is a `ring_node()', otherwise `false'
-spec is_node(X :: (ring_node() | term())) -> boolean().
is_node(X) -> is_record(X, ?NODE).

%% @doc Gets the key of `Node'
-spec get_key(Node :: ring_node()) -> key().
get_key(#?NODE{key = Key}) -> Key.

%% @doc Gets the data of `Node'
-spec get_data(Node :: ring_node()) -> data().
get_data(#?NODE{data = Data}) -> Data.

%% @doc Gets the weight of `Node'
-spec get_weight(Node :: ring_node()) -> weight().
get_weight(#?NODE{weight = Weight}) -> Weight.
