

# Module hash_ring_node #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

An object which represents a node on a consistent hash ring.

Copyright (c) 2013-2016 Takeru Ohta <phjgt308@gmail.com>

<a name="types"></a>

## Data Types ##




### <a name="type-data">data()</a> ###


<pre><code>
data() = term()
</code></pre>

 The data of a `ring_node()`.

It holds arbitrary user data.



### <a name="type-key">key()</a> ###


<pre><code>
key() = term()
</code></pre>

 The key of a `ring_node()`.

It is used to decide location of the node on a ring.



### <a name="type-option">option()</a> ###


<pre><code>
option() = {weight, <a href="#type-weight">weight()</a>}
</code></pre>

 weight:
- A coefficient which is used to determine the virtual node count of the node.
- The higher the value, the number of virtual nodes increases, likely to be more selected.
- The default value is `1`.



### <a name="type-options">options()</a> ###


<pre><code>
options() = [<a href="#type-option">option()</a>]
</code></pre>




### <a name="type-ring_node">ring_node()</a> ###


__abstract datatype__: `ring_node()`

 A node on a ring.



### <a name="type-weight">weight()</a> ###


<pre><code>
weight() = number()
</code></pre>

 The non negative weight of a `ring_node()`.

The more weight node occupies, the more space in a ring.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_data-1">get_data/1</a></td><td>Gets the data of <code>Node</code></td></tr><tr><td valign="top"><a href="#get_key-1">get_key/1</a></td><td>Gets the key of <code>Node</code></td></tr><tr><td valign="top"><a href="#get_weight-1">get_weight/1</a></td><td>Gets the weight of <code>Node</code></td></tr><tr><td valign="top"><a href="#is_node-1">is_node/1</a></td><td>Returns <code>true</code> if <code>X</code> is a <code>ring_node()</code>, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Equivalent to <a href="#make-2"><tt>make(Key, Key)</tt></a>.</td></tr><tr><td valign="top"><a href="#make-2">make/2</a></td><td>Equivalent to <a href="#make-3"><tt>make(Key, Data, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#make-3">make/3</a></td><td>Creates a new <code>ring_node()</code> object.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_data-1"></a>

### get_data/1 ###

<pre><code>
get_data(Node::<a href="#type-ring_node">ring_node()</a>) -&gt; <a href="#type-data">data()</a>
</code></pre>
<br />

Gets the data of `Node`

<a name="get_key-1"></a>

### get_key/1 ###

<pre><code>
get_key(Node::<a href="#type-ring_node">ring_node()</a>) -&gt; <a href="#type-key">key()</a>
</code></pre>
<br />

Gets the key of `Node`

<a name="get_weight-1"></a>

### get_weight/1 ###

<pre><code>
get_weight(Node::<a href="#type-ring_node">ring_node()</a>) -&gt; <a href="#type-weight">weight()</a>
</code></pre>
<br />

Gets the weight of `Node`

<a name="is_node-1"></a>

### is_node/1 ###

<pre><code>
is_node(X::<a href="#type-ring_node">ring_node()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a `ring_node()`, otherwise `false`

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Key::<a href="#type-key">key()</a>) -&gt; <a href="#type-ring_node">ring_node()</a>
</code></pre>
<br />

Equivalent to [`make(Key, Key)`](#make-2).

<a name="make-2"></a>

### make/2 ###

<pre><code>
make(Key::<a href="#type-key">key()</a>, Data::<a href="#type-data">data()</a>) -&gt; <a href="#type-ring_node">ring_node()</a>
</code></pre>
<br />

Equivalent to [`make(Key, Data, [])`](#make-3).

<a name="make-3"></a>

### make/3 ###

<pre><code>
make(Key::<a href="#type-key">key()</a>, Data::<a href="#type-data">data()</a>, Options::<a href="#type-options">options()</a>) -&gt; <a href="#type-ring_node">ring_node()</a>
</code></pre>
<br />

Creates a new `ring_node()` object

