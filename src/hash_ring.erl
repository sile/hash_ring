%% @copyright 2013-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Consistent Hash Rings
%% @end
-module(hash_ring).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([make/1, make/2]).
-export([is_ring/1]).
-export([add_node/2]).
-export([add_nodes/2]).
-export([remove_node/2]).
-export([remove_nodes/2]).
-export([get_nodes/1]).
-export([get_node_list/1]).
-export([get_node_count/1]).
-export([fold/4]).
-export([find_node/2]).
-export([collect_nodes/3]).
-export([list_to_nodes/1]).

-export_type([ring/0, ring/1, ring/2]).
-export_type([ring_node/0]).
-export_type([item/0]).
-export_type([hash_ring_module/0]).
-export_type([options/0, option/0]).
-export_type([hash_algorithm/0]).
-export_type([fold_fun/0]).
-export_type([node_map/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback make([ring_node()], options()) -> impl_state().
-callback add_nodes([ring_node()], impl_state()) -> impl_state().
-callback remove_nodes([ring_node()], impl_state()) -> impl_state().
-callback get_nodes(impl_state()) -> node_map().
-callback fold(fold_fun(), item(), AccIn::term(), impl_state()) -> AccOut::term().

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(DEFAULT_HASH_RING_MODULE, hash_ring_static).

-define(RING, ?MODULE).
-record(?RING,
        {
          impl_module :: hash_ring_module(),
          impl_state  :: impl_state()
        }).

-type ring()              :: ring(hash_ring_node:key()).
-type ring(Key)           :: ring(Key, Key).
-opaque ring(_Key, _Data) :: #?RING{}.
%% A consistent hash ring.
%%
%% It is a unidirection ring.

-type ring_node() :: hash_ring_node:ring_node().
%% A node on a ring

-type item() :: hash_ring_node:key().
%% An item.
%%
%% All items have a virtual address (location) on a ring.

-type hash_ring_module() :: hash_ring_static | hash_ring_dynamic.
%% A `hash_ring' behaviour implementing module

-type impl_state() :: term().
%% The state of an instance of a `hash_ring' implementation module

-type options() :: [option()].
-type option() :: {module, hash_ring_module()}
                | {virtual_node_count, pos_integer()}
                | {max_hash_byte_size, pos_integer()}
                | {hash_algorithm, hash_algorithm()}
                | hash_ring_static:option().
%% module:
%% - The `hash_ring' implementation module which will be used to create and manipulate a hash ring instance.
%% - The default value is `hash_ring_static'.
%%
%% TODO: (virtual_node_count * weight is real ...)
%%
%% Others are implementation specific options.

-type hash_algorithm() :: crc32 | phash2 | md5 | sha | sha256.
%% The hash algorithm which is used to determine locations of nodes and items on a ring

-type fold_fun() :: fun ((ring_node(), Acc::term()) -> {Continue::boolean(), AccNext::term()}).
%% A folding function.
%%
%% `Acc' and `AccNext' are ordinary accumulation variables.
%%
%% If `Continue' is `true' and there are untraversed nodes,
%% a next node will be folded, otherwise the folding will break.

-type node_map() :: #{hash_ring_node:key() => ring_node()}.
%% The map representation of nodes

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv make(Nodes, [])
-spec make([ring_node()]) -> ring().
make(Nodes) ->
    make(Nodes, []).

%% @doc Creates a new consistent hash ring instance
%%
%% ```
%% > R = hash_ring:make(hash_ring:list_to_nodes([a, b, c, d, e])).
%% > hash_ring:get_node_list(R).
%% [{hash_ring_node,a,a,1},
%%  {hash_ring_node,b,b,1},
%%  {hash_ring_node,c,c,1},
%%  {hash_ring_node,d,d,1},
%%  {hash_ring_node,e,e,1}]
%% '''
-spec make([ring_node()], options()) -> ring().
make(Nodes, Options) ->
    Module = proplists:get_value(module, Options, ?DEFAULT_HASH_RING_MODULE),
    State = Module:make(Nodes, Options),
    #?RING{impl_module = Module, impl_state = State}.

%% @doc Returns `true' if `X' is a `ring()', otherwise `false'
-spec is_ring(X :: (ring() | term())) -> boolean().
is_ring(X) -> is_record(X, ?RING).

%% @equiv add_nodes([Node], Ring)
-spec add_node(ring_node(), ring()) -> ring().
add_node(Node, Ring) ->
    add_nodes([Node], Ring).

%% @doc Adds `Nodes' to `Ring'
%%
%% Existing nodes which have the same key will be overwritten.
%%
%% ```
%% > R0 = hash_ring:make([]).
%% > R1 = hash_ring:add_nodes([hash_ring_node:make(a)], R0).
%% > hash_ring:get_node_list(R1).
%% [{hash_ring_node,a,a,1}]
%% '''
-spec add_nodes([ring_node()], ring()) -> ring().
add_nodes(Nodes, Ring) ->
    #?RING{impl_module = Module, impl_state = State0} = Ring,
    State1 = Module:add_nodes(Nodes, State0),
    Ring#?RING{impl_state = State1}.

%% @equiv remove_nodes([Node], Ring)
-spec remove_node(hash_ring_node:key(), ring()) -> ring().
remove_node(Node, Ring) ->
    remove_nodes([Node], Ring).

%% @doc Removes nodes which have a key included in `Keys' from `Ring'
%%
%% ```
%% > R0 = hash_ring:make(hash_ring:list_to_nodes([a, b])).
%% > R1 = hash_ring:remove_nodes([b], R0).
%% > hash_ring:get_node_list(R1).
%% [{hash_ring_node,a,a,1}]
%% '''
-spec remove_nodes([hash_ring_node:key()], ring()) -> ring().
remove_nodes(Keys, Ring) ->
    #?RING{impl_module = Module, impl_state = State0} = Ring,
    State1 = Module:remove_nodes(Keys, State0),
    Ring#?RING{impl_state = State1}.

%% @doc Returns the nodes in `Ring' as a newly created list
-spec get_node_list(ring()) -> [ring_node()].
get_node_list(Ring) ->
    maps:values(get_nodes(Ring)).

%% @doc Returns the nodes in `Ring' as a map
%%
%% Unlike {@link get_nodes/1}, this function requires no memory allocation for the return value.
-spec get_nodes(ring()) -> node_map().
get_nodes(Ring) ->
    #?RING{impl_module = Module, impl_state = State} = Ring,
    Module:get_nodes(State).

%% @doc Returns the number of nodes in `Ring'
-spec get_node_count(ring()) -> non_neg_integer().
get_node_count(Ring) ->
    maps:size(get_nodes(Ring)).

%% @doc Folds `Fun' over every node in `Ring' returning the final value of the accumulator
%%
%% The iteration is started from a node which logically placed at next location of `Item' on `Ring'.
%% And it proceeds clockwise.
%%
%% See also the documentation of `fold_fun()' type.
%%
%% ```
%% %%
%% %% Finds a node which is adjacent to the specified item (cf. `find_node/1').
%% %%
%% > Ring = hash_ring:make(hash_ring:list_to_nodes(lists:seq(1, 100))).
%%
%% > hash_ring:fold(fun (N, _) -> {false, {ok, N}} end, a, error, Ring).
%% {ok, {hash_ring_node,36,36,1}}
%%
%% > hash_ring:fold(fun (N, _) -> {false, {ok, N}} end, b, error, Ring).
%% {ok, {hash_ring_node,53,53,1}}
%% '''
-spec fold(fold_fun(), item(), term(), ring()) -> AccOut::term().
fold(Fun, Item, AccIn, Ring) ->
    #?RING{impl_module = Module, impl_state = State} = Ring,
    Module:fold(Fun, Item, AccIn, State).

%% @doc Finds a node which is logically adjacent to `Item' on `Ring'
%%
%% In other words, it returns `Node' which is the successor of `Item'.
%%
%% If the ring is empty, it returns `error'.
%%
%% ```
%% > Ring = hash_ring:make(hash_ring:list_to_nodes(lists:seq(1, 100))).
%%
%% > hash_ring:find_node(a, Ring).
%% {ok, {hash_ring_node,36,36,1}}
%%
%% > hash_ring:find_node(b, Ring).
%% {ok, {hash_ring_node,53,53,1}}
%% '''
-spec find_node(item(), ring()) -> {ok, Node :: ring_node()} | error.
find_node(Item, Ring) ->
    fold(fun (Node, _) -> {false, {ok, Node}} end,
         Item,
         error,
         Ring).

%% @doc Collects the `N' nodes which are logically adjacent to `Item' on `Ring'
%%
%% In other words, it returns `Nodes' which are the `N' successor of `Item'.
%%
%% If the number of nodes in `Ring' is less than `N',
%% it returns a list which contains all nodes in `Ring',
%% but the order of the elements depdends on `Item'.
%%
%% ```
%% > Ring = hash_ring:make(hash_ring:list_to_nodes([a, b, c, d, e])).
%%
%% > hash_ring:collect_nodes(foo, 3, Ring).
%% [{hash_ring_node,e,e,1},
%%  {hash_ring_node,d,d,1},
%%  {hash_ring_node,a,a,1}]
%%
%% > hash_ring:collect_nodes(bar, 10, Ring).
%% [{hash_ring_node,b,b,1},
%%  {hash_ring_node,c,c,1},
%%  {hash_ring_node,e,e,1},
%%  {hash_ring_node,d,d,1},
%%  {hash_ring_node,a,a,1}]
%%
%% > hash_ring:collect_nodes(baz, 10, Ring).
%% [{hash_ring_node,b,b,1},
%%  {hash_ring_node,a,a,1},
%%  {hash_ring_node,e,e,1},
%%  {hash_ring_node,d,d,1},
%%  {hash_ring_node,c,c,1}]
%% '''
-spec collect_nodes(item(), non_neg_integer(), ring()) -> Nodes :: [ring_node()].
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

%% @doc Creates a list of `ring_node()` from a list of `X'
%%
%% ```
%% > Nodes0 = hash_ring:list_to_nodes([a, {b, 1}, {c, 2, [{weight, 0.2}]}]).
%% > Nodes1 = [hash_ring_node:make(a), hash_ring_node:make(b, 1), hash_ring_node:make(c, 2, [{weight, 0.2}])].
%% > Nodes0 = Nodes1.
%% '''
-spec list_to_nodes([X]) -> [ring_node()] when
      X :: hash_ring_node:key()
         | {hash_ring_node:key(), hash_ring_node:data()}
         | {hash_ring_node:key(), hash_ring_node:data(), hash_ring_node:options()}.
list_to_nodes(List) ->
    [case X of
         {Key, Data, Options} -> hash_ring_node:make(Key, Data, Options);
         {Key, Data}          -> hash_ring_node:make(Key, Data);
         Key                  -> hash_ring_node:make(Key)
     end || X <- List].
