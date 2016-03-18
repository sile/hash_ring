

# Module hash_ring #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Consistent Hash Rings.

Copyright (c) 2013-2016 Takeru Ohta <phjgt308@gmail.com>

__This module defines the `hash_ring` behaviour.__<br /> Required callback functions: `make/2`, `is_ring/1`, `add_nodes/2`, `remove_nodes/2`, `get_nodes/1`, `get_node_count/1`, `fold/4`.

<a name="types"></a>

## Data Types ##




### <a name="type-fold_fun">fold_fun()</a> ###


<pre><code>
fold_fun() = fun((<a href="#type-ring_node">ring_node()</a>, Acc::term()) -&gt; {Continue::boolean(), AccNext::term()})
</code></pre>

 A folding function.

`Acc` and `AccNext` are ordinary accumulation variables.

If `Continue` is `true` and there are untraversed nodes,
a next node will be folded, otherwise the folding will break.



### <a name="type-hash_algorithms">hash_algorithms()</a> ###


<pre><code>
hash_algorithms() = crc32 | phash2 | md5 | sha | sha256
</code></pre>

 The hash algorithm which is used to determine locations of nodes and items on a ring



### <a name="type-hash_ring_module">hash_ring_module()</a> ###


<pre><code>
hash_ring_module() = hash_ring_static | hash_ring_dynamic
</code></pre>

 A `hash_ring` behaviour implementing module



### <a name="type-item">item()</a> ###


<pre><code>
item() = <a href="hash_ring_node.md#type-key">hash_ring_node:key()</a>
</code></pre>




### <a name="type-option">option()</a> ###


<pre><code>
option() = {module, <a href="#type-hash_ring_module">hash_ring_module()</a>} | <a href="hash_ring_static.md#type-option">hash_ring_static:option()</a> | <a href="hash_ring_dynamic.md#type-option">hash_ring_dynamic:option()</a>
</code></pre>

 module:
- The `hash_ring` implementation module which will be used to create and manipulate a hash ring instance.
- The default value is `hash_ring_static`.

Others are implementation specific options.



### <a name="type-options">options()</a> ###


<pre><code>
options() = [<a href="#type-option">option()</a>]
</code></pre>




### <a name="type-ring">ring()</a> ###


__abstract datatype__: `ring(_Key, _Data)`

 A consistent hash ring.

It is a unidirection ring.



### <a name="type-ring">ring()</a> ###


<pre><code>
ring(Key) = <a href="#type-ring">ring</a>(Key, Key)
</code></pre>




### <a name="type-ring">ring()</a> ###


<pre><code>
ring() = <a href="#type-ring">ring</a>(<a href="hash_ring_node.md#type-key">hash_ring_node:key()</a>)
</code></pre>




### <a name="type-ring_node">ring_node()</a> ###


<pre><code>
ring_node() = <a href="hash_ring_node.md#type-ring_node">hash_ring_node:ring_node()</a>
</code></pre>

 A node on a ring

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_node-2">add_node/2</a></td><td>Equivalent to <a href="#add_nodes-2"><tt>add_nodes([Node], Ring)</tt></a>.</td></tr><tr><td valign="top"><a href="#add_nodes-2">add_nodes/2</a></td><td>Adds <code>Nodes</code> to <code>Ring</code></td></tr><tr><td valign="top"><a href="#collect_nodes-3">collect_nodes/3</a></td><td>Collects the <code>N</code> nodes which are logically adjacent to <code>Item</code> on <code>Ring</code></td></tr><tr><td valign="top"><a href="#find_node-2">find_node/2</a></td><td>Finds a node which is logically adjacent to <code>Item</code> on <code>Ring</code></td></tr><tr><td valign="top"><a href="#fold-4">fold/4</a></td><td>Folds <code>Fun</code> over every node in <code>Ring</code> returning the final value of the accumulator.</td></tr><tr><td valign="top"><a href="#get_node_count-1">get_node_count/1</a></td><td>Returns the number of nodes in <code>Ring</code></td></tr><tr><td valign="top"><a href="#get_nodes-1">get_nodes/1</a></td><td>Returns the nodes in <code>Ring</code> as a list.</td></tr><tr><td valign="top"><a href="#is_ring-1">is_ring/1</a></td><td>Returns <code>true</code> if <code>X</code> is a <code>ring()</code>, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#list_to_nodes-1">list_to_nodes/1</a></td><td>Creates a list of <code>ring_node()` from a list of `X</code></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Equivalent to <a href="#make-2"><tt>make(Nodes, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#make-2">make/2</a></td><td>Creates a new consistent hash ring instance.</td></tr><tr><td valign="top"><a href="#remove_node-2">remove_node/2</a></td><td>Equivalent to <a href="#remove_nodes-2"><tt>remove_nodes([Node], Ring)</tt></a>.</td></tr><tr><td valign="top"><a href="#remove_nodes-2">remove_nodes/2</a></td><td>Removes nodes which have a key included in <code>Keys</code> from <code>Ring</code></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_node-2"></a>

### add_node/2 ###

<pre><code>
add_node(Node::<a href="#type-ring_node">ring_node()</a>, Ring::<a href="#type-ring">ring()</a>) -&gt; <a href="#type-ring">ring()</a>
</code></pre>
<br />

Equivalent to [`add_nodes([Node], Ring)`](#add_nodes-2).

<a name="add_nodes-2"></a>

### add_nodes/2 ###

<pre><code>
add_nodes(Nodes::[<a href="#type-ring_node">ring_node()</a>], Ring::<a href="#type-ring">ring()</a>) -&gt; <a href="#type-ring">ring()</a>
</code></pre>
<br />

Adds `Nodes` to `Ring`

Existing nodes which have the same key will be overwritten.

```
  > R0 = hash_ring:make([]).
  > R1 = hash_ring:add_nodes([hash_ring_node:make(a)], R0).
  > hash_ring:get_nodes(R1).
  [{hash_ring_node,a,a,1}]
```

<a name="collect_nodes-3"></a>

### collect_nodes/3 ###

<pre><code>
collect_nodes(Item::<a href="#type-item">item()</a>, N::non_neg_integer(), Ring::<a href="#type-ring">ring()</a>) -&gt; Nodes::[<a href="#type-ring_node">ring_node()</a>]
</code></pre>
<br />

Collects the `N` nodes which are logically adjacent to `Item` on `Ring`

In other words, it returns `Nodes` which are the `N` successor of `Item`.

If the number of nodes in `Ring` is less than `N`,
it returns a list which contains all nodes in `Ring`,
but the order of the elements depdends on `Item`.

```
  > Ring = hash_ring:make(hash_ring:list_to_nodes([a, b, c, d, e])).
  > hash_ring:collect_nodes(foo, 3, Ring).
  [{hash_ring_node,e,e,1},
   {hash_ring_node,d,d,1},
   {hash_ring_node,a,a,1}]
  > hash_ring:collect_nodes(bar, 10, Ring).
  [{hash_ring_node,b,b,1},
   {hash_ring_node,c,c,1},
   {hash_ring_node,e,e,1},
   {hash_ring_node,d,d,1},
   {hash_ring_node,a,a,1}]
  > hash_ring:collect_nodes(baz, 10, Ring).
  [{hash_ring_node,b,b,1},
   {hash_ring_node,a,a,1},
   {hash_ring_node,e,e,1},
   {hash_ring_node,d,d,1},
   {hash_ring_node,c,c,1}]
```

<a name="find_node-2"></a>

### find_node/2 ###

<pre><code>
find_node(Item::<a href="#type-item">item()</a>, Ring::<a href="#type-ring">ring()</a>) -&gt; {ok, Node::<a href="#type-ring_node">ring_node()</a>} | error
</code></pre>
<br />

Finds a node which is logically adjacent to `Item` on `Ring`

In other words, it returns `Node` which is the successor of `Item`.

If the ring is empty, it returns `error`.

```
  > Ring = hash_ring:make(hash_ring:list_to_nodes(lists:seq(1, 100))).
  > hash_ring:find_node(a, Ring).
  {ok, {hash_ring_node,36,36,1}}
  > hash_ring:find_node(b, Ring).
  {ok, {hash_ring_node,53,53,1}}
```

<a name="fold-4"></a>

### fold/4 ###

<pre><code>
fold(Fun::<a href="#type-fold_fun">fold_fun()</a>, Item::<a href="#type-item">item()</a>, AccIn::term(), Ring::<a href="#type-ring">ring()</a>) -&gt; AccOut::term()
</code></pre>
<br />

Folds `Fun` over every node in `Ring` returning the final value of the accumulator

The iteration is started from a node which logically placed at next location of `Item` on `Ring`.
And it proceeds clockwise.

See also the documentation of `fold_fun()` type.

```
  %%
  %% Finds a node which is adjacent to the specified item (cf. `find_node/1').
  %%
  > Ring = hash_ring:make(hash_ring:list_to_nodes(lists:seq(1, 100))).
  > hash_ring:fold(fun (N, _) -> {false, {ok, N}} end, a, error, Ring).
  {ok, {hash_ring_node,36,36,1}}
  > hash_ring:fold(fun (N, _) -> {false, {ok, N}} end, b, error, Ring).
  {ok, {hash_ring_node,53,53,1}}
```

<a name="get_node_count-1"></a>

### get_node_count/1 ###

<pre><code>
get_node_count(Ring::<a href="#type-ring">ring()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

Returns the number of nodes in `Ring`

<a name="get_nodes-1"></a>

### get_nodes/1 ###

<pre><code>
get_nodes(Ring::<a href="#type-ring">ring()</a>) -&gt; [<a href="#type-ring_node">ring_node()</a>]
</code></pre>
<br />

Returns the nodes in `Ring` as a list

<a name="is_ring-1"></a>

### is_ring/1 ###

<pre><code>
is_ring(X::<a href="#type-ring">ring()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a `ring()`, otherwise `false`

<a name="list_to_nodes-1"></a>

### list_to_nodes/1 ###

<pre><code>
list_to_nodes(X::[X]) -&gt; [<a href="#type-ring_node">ring_node()</a>]
</code></pre>

<ul class="definitions"><li><code>X = <a href="hash_ring_node.md#type-key">hash_ring_node:key()</a> | {<a href="hash_ring_node.md#type-key">hash_ring_node:key()</a>, <a href="hash_ring_node.md#type-data">hash_ring_node:data()</a>} | {<a href="hash_ring_node.md#type-key">hash_ring_node:key()</a>, <a href="hash_ring_node.md#type-data">hash_ring_node:data()</a>, <a href="hash_ring_node.md#type-options">hash_ring_node:options()</a>}</code></li></ul>

Creates a list of `ring_node()` from a list of `X`

```
  > Nodes0 = hash_ring:list_to_nodes([a, {b, 1}, {c, 2, [{weight, 0.2}]}]).
  > Nodes1 = [hash_ring_node:make(a), hash_ring_node:make(b, 1), hash_ring_node:make(c, 2, [{weight, 0.2}])].
  > Nodes0 = Nodes1.
```

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Nodes::[<a href="#type-ring_node">ring_node()</a>]) -&gt; <a href="#type-ring">ring()</a>
</code></pre>
<br />

Equivalent to [`make(Nodes, [])`](#make-2).

<a name="make-2"></a>

### make/2 ###

<pre><code>
make(Nodes::[<a href="#type-ring_node">ring_node()</a>], Options::<a href="#type-options">options()</a>) -&gt; <a href="#type-ring">ring()</a>
</code></pre>
<br />

Creates a new consistent hash ring instance

```
  > R = hash_ring:make(hash_ring:list_to_nodes([a, b, c, d, e])).
  > hash_ring:get_nodes(R).
  [{hash_ring_node,a,a,1},
   {hash_ring_node,b,b,1},
   {hash_ring_node,c,c,1},
   {hash_ring_node,d,d,1},
   {hash_ring_node,e,e,1}]
```

<a name="remove_node-2"></a>

### remove_node/2 ###

<pre><code>
remove_node(Node::<a href="hash_ring_node.md#type-key">hash_ring_node:key()</a>, Ring::<a href="#type-ring">ring()</a>) -&gt; <a href="#type-ring">ring()</a>
</code></pre>
<br />

Equivalent to [`remove_nodes([Node], Ring)`](#remove_nodes-2).

<a name="remove_nodes-2"></a>

### remove_nodes/2 ###

<pre><code>
remove_nodes(Keys::[<a href="hash_ring_node.md#type-key">hash_ring_node:key()</a>], Ring::<a href="#type-ring">ring()</a>) -&gt; <a href="#type-ring">ring()</a>
</code></pre>
<br />

Removes nodes which have a key included in `Keys` from `Ring`

```
  > R0 = hash_ring:make(hash_ring:list_to_nodes([a, b])).
  > R1 = hash_ring:remove_nodes([b], R0).
  > hash_ring:get_nodes(R1).
  [{hash_ring_node,a,a,1}]
```

