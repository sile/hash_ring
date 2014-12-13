

# Module hash_ring #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


[12467,12531,12471,12473,12486,12531,12488,12495,12483,12471,12517,12522,
 12531,12464,25805,20316,29992,12398,12452,12531,12479,12540,12501,12455,
 12540,12473,12514,12472,12517,12540,12523,46]
Copyright (c) 2013-2014 Takeru Ohta <phjgt308@gmail.com>


__This module defines the `hash_ring` behaviour.__
<br></br>
 Required callback functions: `make/2`, `is_ring/1`, `add_nodes/2`, `remove_nodes/2`, `get_nodes/1`, `get_node_count/1`, `fold/4`.

<a name="types"></a>

## Data Types ##




### <a name="type-fold_fun">fold_fun()</a> ###



<pre><code>
fold_fun() = fun((<a href="#type-ring_node">ring_node()</a>, Acc::term()) -&gt; {Continue::boolean(), AccNext::term()})
</code></pre>





### <a name="type-hash_algorithms">hash_algorithms()</a> ###



<pre><code>
hash_algorithms() = crc32 | phash2 | md5 | sha | sha256
</code></pre>





### <a name="type-hash_ring_module">hash_ring_module()</a> ###



<pre><code>
hash_ring_module() = hash_ring_static
</code></pre>





### <a name="type-item">item()</a> ###



<pre><code>
item() = term()
</code></pre>





### <a name="type-option">option()</a> ###



<pre><code>
option() = {module, <a href="#type-hash_ring_module">hash_ring_module()</a>} | <a href="hash_ring_static.md#type-option">hash_ring_static:option()</a>
</code></pre>





### <a name="type-ring">ring()</a> ###


__abstract datatype__: `ring()`




### <a name="type-ring_node">ring_node()</a> ###



<pre><code>
ring_node() = <a href="hash_ring_node.md#type-ring_node">hash_ring_node:ring_node()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_nodes-2">add_nodes/2</a></td><td>[12494,12540,12489,32676,12434,36861,21152,12377,12427,46]</td></tr><tr><td valign="top"><a href="#collect_nodes-3">collect_nodes/3</a></td><td>[25351,23450,12398,12450,12452,12486,12512,12434,25285,24403,12377,12427,
 12494,12540,12489,12434,20778,20808,38918,20301,12364,39640,12356,38918,
 12395,26368,22823]<code>N</code>[20491,38598,12417,12427,46]</td></tr><tr><td valign="top"><a href="#find_node-2">find_node/2</a></td><td>[25351,23450,12398,12450,12452,12486,12512,12434,25285,24403,12377,12427,
 12494,12540,12489,12434,26908,32034,12377,12427,46]</td></tr><tr><td valign="top"><a href="#fold-4">fold/4</a></td><td>[12450,12452,12486,12512,12398,27425,12395,20301,32622,12377,12427,12494,
 12540,12489,12363,12425,38918,12395,30067,12415,36796,12415,12434,34892,
 12358,46]</td></tr><tr><td valign="top"><a href="#get_node_count-1">get_node_count/1</a></td><td>[12494,12540,12489,12398,20491,25968,12434,21462,24471,12377,12427,46]</td></tr><tr><td valign="top"><a href="#get_nodes-1">get_nodes/1</a></td><td>[12494,12540,12489,19968,35239,12434,21462,24471,12377,12427,46]</td></tr><tr><td valign="top"><a href="#is_ring-1">is_ring/1</a></td><td>[24341,25968,12398,20516,12364,36969,20999,12395,29983,25104,12373,12428,
 12383,12522,12531,12464,12458,12502,12472,12455,12463,12488,12363,12393,
 12358,12363,12434,21028,23450,12377,12427,46]</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>equiv make(Nodes, []).</td></tr><tr><td valign="top"><a href="#make-2">make/2</a></td><td>[12467,12531,12471,12473,12486,12531,12488,12495,12483,12471,12517,12522,
 12531,12464,12434,27083,31689,12377,12427,46]</td></tr><tr><td valign="top"><a href="#remove_nodes-2">remove_nodes/2</a></td><td>[12461,12540,12395,23550,24540,12377,12427,12494,12540,12489,32676,12434,
 21066,38500,12377,12427,46]</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_nodes-2"></a>

### add_nodes/2 ###


<pre><code>
add_nodes(Nodes::[<a href="#type-ring_node">ring_node()</a>], Ring::<a href="#type-ring">ring()</a>) -&gt; <a href="#type-ring">ring()</a>
</code></pre>

<br></br>



[12494,12540,12489,32676,12434,36861,21152,12377,12427]


[12461,12540,12364,31561,12375,12356,12494,12540,12489,12364,26082,12395,
 23384,22312,12377,12427,22580,21512,12399,12289,19978,26360,12365,12373,
 12428,12427]
<a name="collect_nodes-3"></a>

### collect_nodes/3 ###


<pre><code>
collect_nodes(Item::<a href="#type-item">item()</a>, N::non_neg_integer(), Ring::<a href="#type-ring">ring()</a>) -&gt; [<a href="#type-ring_node">ring_node()</a>]
</code></pre>

<br></br>


[25351,23450,12398,12450,12452,12486,12512,12434,25285,24403,12377,12427,
 12494,12540,12489,12434,20778,20808,38918,20301,12364,39640,12356,38918,
 12395,26368,22823]`N`[20491,38598,12417,12427]
<a name="find_node-2"></a>

### find_node/2 ###


<pre><code>
find_node(Item::<a href="#type-item">item()</a>, Ring::<a href="#type-ring">ring()</a>) -&gt; {ok, <a href="#type-ring_node">ring_node()</a>} | error
</code></pre>

<br></br>



[25351,23450,12398,12450,12452,12486,12512,12434,25285,24403,12377,12427,
 12494,12540,12489,12434,26908,32034,12377,12427]


[12522,12531,12464,12364,31354,12398,22580,21512,12399]`error`[12364,36820,12373,12428,12427]
<a name="fold-4"></a>

### fold/4 ###


<pre><code>
fold(Fun::<a href="#type-fold_fun">fold_fun()</a>, Item::<a href="#type-item">item()</a>, Initial::term(), Ring::<a href="#type-ring">ring()</a>) -&gt; Result::term()
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


[12494,12540,12489,12398,20491,25968,12434,21462,24471,12377,12427]
<a name="get_nodes-1"></a>

### get_nodes/1 ###


<pre><code>
get_nodes(Ring::<a href="#type-ring">ring()</a>) -&gt; [<a href="#type-ring_node">ring_node()</a>]
</code></pre>

<br></br>



[12494,12540,12489,19968,35239,12434,21462,24471,12377,12427]


[36820,12426,20516,12398,12494,12540,12489,12399,12461,12540,38918,12395,
 26119,38918,12395,12477,12540,12488,12373,12428,12390,12356,12427]
<a name="is_ring-1"></a>

### is_ring/1 ###


<pre><code>
is_ring(Hash_ring::<a href="#type-ring">ring()</a> | term()) -&gt; boolean()
</code></pre>

<br></br>


[24341,25968,12398,20516,12364,36969,20999,12395,29983,25104,12373,12428,
 12383,12522,12531,12464,12458,12502,12472,12455,12463,12488,12363,12393,
 12358,12363,12434,21028,23450,12377,12427]
<a name="make-1"></a>

### make/1 ###


<pre><code>
make(Nodes::[<a href="#type-ring_node">ring_node()</a>]) -&gt; <a href="#type-ring">ring()</a>
</code></pre>

<br></br>


equiv make(Nodes, [])
<a name="make-2"></a>

### make/2 ###


<pre><code>
make(Nodes::[<a href="#type-ring_node">ring_node()</a>], Options::[<a href="#type-option">option()</a>]) -&gt; <a href="#type-ring">ring()</a>
</code></pre>

<br></br>


[12467,12531,12471,12473,12486,12531,12488,12495,12483,12471,12517,12522,
 12531,12464,12434,27083,31689,12377,12427]
<a name="remove_nodes-2"></a>

### remove_nodes/2 ###


<pre><code>
remove_nodes(Nodes::[<a href="hash_ring_node.md#type-key">hash_ring_node:key()</a>], Ring::<a href="#type-ring">ring()</a>) -&gt; <a href="#type-ring">ring()</a>
</code></pre>

<br></br>


[12461,12540,12395,23550,24540,12377,12427,12494,12540,12489,32676,12434,
 21066,38500,12377,12427]
