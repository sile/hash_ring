

# Module hash_ring_node #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


コンシステントハッシュリング上のノードを表現するオブジェクト.
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



  weight: <br />
- 仮想ノードの個数を決定する際の重み <br />
- 値が大きいほど仮想ノードの個数が多くなり、より選択されやすくなる <br />
- デフォルト値: `1.0` <br />



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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#calc_virtual_node_count-2">calc_virtual_node_count/2</a></td><td>仮想ノードの数を計算する.</td></tr><tr><td valign="top"><a href="#get_data-1">get_data/1</a></td><td>ノードのデータを取得する.</td></tr><tr><td valign="top"><a href="#get_key-1">get_key/1</a></td><td>ノードのキーを取得する.</td></tr><tr><td valign="top"><a href="#get_weight-1">get_weight/1</a></td><td>ノードの重みを取得する.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Equivalent to <a href="#make-2"><tt>make(Key, Key)</tt></a>.</td></tr><tr><td valign="top"><a href="#make-2">make/2</a></td><td>Equivalent to <a href="#make-3"><tt>make(Key, Data, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#make-3">make/3</a></td><td><code>ring_node()</code>のインスタンスを生成する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="calc_virtual_node_count-2"></a>

### calc_virtual_node_count/2 ###


<pre><code>
calc_virtual_node_count(BaseVirtualNodeCount::non_neg_integer(), Node::<a href="#type-ring_node">ring_node()</a>) -&gt; VirtualNodeCount::non_neg_integer()
</code></pre>
<br />

仮想ノードの数を計算する
<a name="get_data-1"></a>

### get_data/1 ###


<pre><code>
get_data(Hash_ring_node::<a href="#type-ring_node">ring_node()</a>) -&gt; <a href="#type-data">data()</a>
</code></pre>
<br />

ノードのデータを取得する
<a name="get_key-1"></a>

### get_key/1 ###


<pre><code>
get_key(Hash_ring_node::<a href="#type-ring_node">ring_node()</a>) -&gt; <a href="#type-key">key()</a>
</code></pre>
<br />

ノードのキーを取得する
<a name="get_weight-1"></a>

### get_weight/1 ###


<pre><code>
get_weight(Hash_ring_node::<a href="#type-ring_node">ring_node()</a>) -&gt; <a href="#type-weight">weight()</a>
</code></pre>
<br />

ノードの重みを取得する
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

`ring_node()`のインスタンスを生成する
