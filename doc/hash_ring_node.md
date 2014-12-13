

# Module hash_ring_node #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


[12467,12531,12471,12473,12486,12531,12488,12495,12483,12471,12517,12522,
 12531,12464,19978,12398,12494,12540,12489,12434,34920,29694,12377,12427,
 12458,12502,12472,12455,12463,12488,46]
Copyright (c) 2013-2014 Takeru Ohta <phjgt308@gmail.com>


<a name="types"></a>

## Data Types ##




### <a name="type-data">data()</a> ###



<pre><code>
data() = term()
</code></pre>





### <a name="type-key">key()</a> ###



<pre><code>
key() = term()
</code></pre>





### <a name="type-option">option()</a> ###



<pre><code>
option() = {weight, <a href="#type-weight">weight()</a>}
</code></pre>



  weight: 
<br></br>
[10,32,32,32,45,32,20206,24819,12494,12540,12489,12398,20491,25968,12434,
 27770,23450,12377,12427,38555,12398,37325,12415,32]
<br></br>
[10,32,32,32,45,32,20516,12364,22823,12365,12356,12411,12393,20206,24819,
 12494,12540,12489,12398,20491,25968,12364,22810,12367,12394,12426,12289,
 12424,12426,36984,25246,12373,12428,12420,12377,12367,12394,12427,32]
<br></br>
[10,32,32,32,45,32,12487,12501,12457,12523,12488,20516,58,32]`1.0` 
<br></br>




### <a name="type-options">options()</a> ###



<pre><code>
options() = [<a href="#type-option">option()</a>]
</code></pre>





### <a name="type-ring_node">ring_node()</a> ###


__abstract datatype__: `ring_node()`




### <a name="type-weight">weight()</a> ###



<pre><code>
weight() = number()
</code></pre>



 non negative number
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#calc_virtual_node_count-2">calc_virtual_node_count/2</a></td><td>[20206,24819,12494,12540,12489,12398,25968,12434,35336,31639,12377,12427,46]</td></tr><tr><td valign="top"><a href="#get_data-1">get_data/1</a></td><td>[12494,12540,12489,12398,12487,12540,12479,12434,21462,24471,12377,12427,46]</td></tr><tr><td valign="top"><a href="#get_key-1">get_key/1</a></td><td>[12494,12540,12489,12398,12461,12540,12434,21462,24471,12377,12427,46]</td></tr><tr><td valign="top"><a href="#get_weight-1">get_weight/1</a></td><td>[12494,12540,12489,12398,37325,12415,12434,21462,24471,12377,12427,46]</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Equivalent to <a href="#make-2"><tt>make(Key, Key)</tt></a>.</td></tr><tr><td valign="top"><a href="#make-2">make/2</a></td><td>Equivalent to <a href="#make-3"><tt>make(Key, Data, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#make-3">make/3</a></td><td><code>ring_node()</code>[12398,12452,12531,12473,12479,12531,12473,12434,29983,25104,12377,12427,46]</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="calc_virtual_node_count-2"></a>

### calc_virtual_node_count/2 ###


<pre><code>
calc_virtual_node_count(BaseVirtualNodeCount::non_neg_integer(), Node::<a href="#type-ring_node">ring_node()</a>) -&gt; VirtualNodeCount::non_neg_integer()
</code></pre>

<br></br>


[20206,24819,12494,12540,12489,12398,25968,12434,35336,31639,12377,12427]
<a name="get_data-1"></a>

### get_data/1 ###


<pre><code>
get_data(Hash_ring_node::<a href="#type-ring_node">ring_node()</a>) -&gt; <a href="#type-data">data()</a>
</code></pre>

<br></br>


[12494,12540,12489,12398,12487,12540,12479,12434,21462,24471,12377,12427]
<a name="get_key-1"></a>

### get_key/1 ###


<pre><code>
get_key(Hash_ring_node::<a href="#type-ring_node">ring_node()</a>) -&gt; <a href="#type-key">key()</a>
</code></pre>

<br></br>


[12494,12540,12489,12398,12461,12540,12434,21462,24471,12377,12427]
<a name="get_weight-1"></a>

### get_weight/1 ###


<pre><code>
get_weight(Hash_ring_node::<a href="#type-ring_node">ring_node()</a>) -&gt; <a href="#type-weight">weight()</a>
</code></pre>

<br></br>


[12494,12540,12489,12398,37325,12415,12434,21462,24471,12377,12427]
<a name="make-1"></a>

### make/1 ###


<pre><code>
make(Key::<a href="#type-key">key()</a>) -&gt; <a href="#type-ring_node">ring_node()</a>
</code></pre>

<br></br>


Equivalent to [`make(Key, Key)`](#make-2).
<a name="make-2"></a>

### make/2 ###


<pre><code>
make(Key::<a href="#type-key">key()</a>, Data::<a href="#type-data">data()</a>) -&gt; <a href="#type-ring_node">ring_node()</a>
</code></pre>

<br></br>


Equivalent to [`make(Key, Data, [])`](#make-3).
<a name="make-3"></a>

### make/3 ###


<pre><code>
make(Key::<a href="#type-key">key()</a>, Data::<a href="#type-data">data()</a>, Options::<a href="#type-options">options()</a>) -&gt; <a href="#type-ring_node">ring_node()</a>
</code></pre>

<br></br>


`ring_node()`[12398,12452,12531,12473,12479,12531,12473,12434,29983,25104,12377,12427]
