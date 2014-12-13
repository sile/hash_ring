

# Module hash_ring_static #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


[38745,30340,12395,27083,31689,12373,12428,12383,12467,12531,12471,12473,
 12486,12531,12488,12495,12483,12471,12517,12522,12531,12464,12434,25805,
 20316,12377,12427,12383,12417,12398,12514,12472,12517,12540,12523,46]
Copyright (c) 2013-2014 Takeru Ohta <phjgt308@gmail.com>


__Behaviours:__ [`hash_ring`](hash_ring.md).

<a name="types"></a>

## Data Types ##




### <a name="type-option">option()</a> ###



<pre><code>
option() = {virtual_node_count, pos_integer()} | {max_hash_byte_size, pos_integer()} | {hash_algorithm, <a href="hash_ring.md#type-hash_algorithms">hash_ring:hash_algorithms()</a>} | {weight_mode, <a href="#type-weight_mode">weight_mode()</a>} | {sentinel_key, <a href="hash_ring_node.md#type-key">hash_ring_node:key()</a>}
</code></pre>





### <a name="type-ring">ring()</a> ###


__abstract datatype__: `ring()`




### <a name="type-weight_mode">weight_mode()</a> ###



<pre><code>
weight_mode() = direct | average
</code></pre>



[32,32,45,32,100,105,114,101,99,116,58,32,25351,23450,12373,12428,12383,37325,
 12415,12434,12381,12398,12414,12414,20351,12358,10,32,32,45,32,97,118,101,
 114,97,103,101,58,32,21508,12494,12540,12489,12398,37325,12415,12434,24179,
 22343,21270,12377,12427]
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_nodes-2">add_nodes/2</a></td><td>[12494,12540,12489,32676,12434,36861,21152,12377,12427,46]</td></tr><tr><td valign="top"><a href="#fold-4">fold/4</a></td><td>[12450,12452,12486,12512,12398,27425,12395,20301,32622,12377,12427,12494,
 12540,12489,12363,12425,38918,12395,30067,12415,36796,12415,12434,34892,
 12358,46]</td></tr><tr><td valign="top"><a href="#get_node_count-1">get_node_count/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_nodes-1">get_nodes/1</a></td><td>[12494,12540,12489,19968,35239,12434,21462,24471,12377,12427,46]</td></tr><tr><td valign="top"><a href="#is_ring-1">is_ring/1</a></td><td>[24341,25968,12398,20516,12364,36969,20999,12395,29983,25104,12373,12428,
 12383,12522,12531,12464,12458,12502,12472,12455,12463,12488,12363,12393,
 12358,12363,12434,21028,23450,12377,12427,46]</td></tr><tr><td valign="top"><a href="#make-2">make/2</a></td><td>[12467,12531,12471,12473,12486,12531,12488,12495,12483,12471,12517,12522,
 12531,12464,12434,27083,31689,12377,12427,46]</td></tr><tr><td valign="top"><a href="#remove_nodes-2">remove_nodes/2</a></td><td>[12494,12540,12489,32676,12434,21066,38500,12377,12427,46]</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_nodes-2"></a>

### add_nodes/2 ###


<pre><code>
add_nodes(Nodes::[<a href="hash_ring.md#type-ring_node">hash_ring:ring_node()</a>], Ring::<a href="#type-ring">ring()</a>) -&gt; <a href="#type-ring">ring()</a>
</code></pre>

<br></br>


[12494,12540,12489,32676,12434,36861,21152,12377,12427]
<a name="fold-4"></a>

### fold/4 ###


<pre><code>
fold(Fun::<a href="hash_ring.md#type-fold_fun">hash_ring:fold_fun()</a>, Item::<a href="hash_ring.md#type-item">hash_ring:item()</a>, Initial::term(), Hash_ring_static::<a href="#type-ring">ring()</a>) -&gt; Result::term()
</code></pre>

<br></br>


[12450,12452,12486,12512,12398,27425,12395,20301,32622,12377,12427,12494,
 12540,12489,12363,12425,38918,12395,30067,12415,36796,12415,12434,34892,
 12358]
<a name="get_node_count-1"></a>

### get_node_count/1 ###


<pre><code>
get_node_count(Ring::<a href="#type-ring">ring()</a>) -&gt; non_neg_integer()
</code></pre>

<br></br>


__See also:__ [hash_ring:get_node_count/1](hash_ring.md#get_node_count-1).
<a name="get_nodes-1"></a>

### get_nodes/1 ###


<pre><code>
get_nodes(Ring::<a href="#type-ring">ring()</a>) -&gt; [<a href="hash_ring.md#type-ring_node">hash_ring:ring_node()</a>]
</code></pre>

<br></br>


[12494,12540,12489,19968,35239,12434,21462,24471,12377,12427]
<a name="is_ring-1"></a>

### is_ring/1 ###


<pre><code>
is_ring(Hash_ring_static::<a href="#type-ring">ring()</a> | term()) -&gt; boolean()
</code></pre>

<br></br>


[24341,25968,12398,20516,12364,36969,20999,12395,29983,25104,12373,12428,
 12383,12522,12531,12464,12458,12502,12472,12455,12463,12488,12363,12393,
 12358,12363,12434,21028,23450,12377,12427]
<a name="make-2"></a>

### make/2 ###


<pre><code>
make(Nodes::[<a href="hash_ring.md#type-ring_node">hash_ring:ring_node()</a>], Options::[<a href="#type-option">option()</a>]) -&gt; <a href="#type-ring">ring()</a>
</code></pre>

<br></br>


[12467,12531,12471,12473,12486,12531,12488,12495,12483,12471,12517,12522,
 12531,12464,12434,27083,31689,12377,12427]
<a name="remove_nodes-2"></a>

### remove_nodes/2 ###


<pre><code>
remove_nodes(Keys::[<a href="hash_ring_node.md#type-key">hash_ring_node:key()</a>], Ring::<a href="#type-ring">ring()</a>) -&gt; <a href="#type-ring">ring()</a>
</code></pre>

<br></br>


[12494,12540,12489,32676,12434,21066,38500,12377,12427]
