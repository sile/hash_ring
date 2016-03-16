

# Module hash_ring_dynamic #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

動的なノード追加・削除に強いhash_ringの実装モジュール.

Copyright (c) 2013-2016 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`hash_ring`](hash_ring.md).

<a name="types"></a>

## Data Types ##




### <a name="type-option">option()</a> ###


<pre><code>
option() = {virtual_node_count, pos_integer()} | {max_hash_byte_size, pos_integer()} | {hash_algorithm, <a href="hash_ring.md#type-hash_algorithms">hash_ring:hash_algorithms()</a>} | {weight_mode, <a href="#type-weight_mode">weight_mode()</a>}
</code></pre>

default: direct



### <a name="type-ring">ring()</a> ###


__abstract datatype__: `ring()`




### <a name="type-weight_mode">weight_mode()</a> ###


<pre><code>
weight_mode() = direct | average
</code></pre>

 - direct: 指定された重みをそのまま使う
- average: 各ノードの重みを平均化する

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_nodes-2">add_nodes/2</a></td><td>ノード群を追加する.</td></tr><tr><td valign="top"><a href="#fold-4">fold/4</a></td><td>アイテムの次に位置するノードから順に畳み込みを行う.</td></tr><tr><td valign="top"><a href="#get_node_count-1">get_node_count/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_nodes-1">get_nodes/1</a></td><td>ノード一覧を取得する.</td></tr><tr><td valign="top"><a href="#is_ring-1">is_ring/1</a></td><td>引数の値が適切に生成されたリングオブジェクトかどうかを判定する.</td></tr><tr><td valign="top"><a href="#make-2">make/2</a></td><td>コンシステントハッシュリングを構築する.</td></tr><tr><td valign="top"><a href="#remove_nodes-2">remove_nodes/2</a></td><td>ノード群を削除する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_nodes-2"></a>

### add_nodes/2 ###

<pre><code>
add_nodes(Nodes::[<a href="hash_ring.md#type-ring_node">hash_ring:ring_node()</a>], Ring::<a href="#type-ring">ring()</a>) -&gt; <a href="#type-ring">ring()</a>
</code></pre>
<br />

ノード群を追加する

<a name="fold-4"></a>

### fold/4 ###

<pre><code>
fold(Fun::<a href="hash_ring.md#type-fold_fun">hash_ring:fold_fun()</a>, Item::<a href="hash_ring.md#type-item">hash_ring:item()</a>, Initial::term(), Ring::<a href="#type-ring">ring()</a>) -&gt; Result::term()
</code></pre>
<br />

アイテムの次に位置するノードから順に畳み込みを行う

<a name="get_node_count-1"></a>

### get_node_count/1 ###

<pre><code>
get_node_count(Ring::<a href="#type-ring">ring()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

__See also:__ [hash_ring:get_node_count/1](hash_ring.md#get_node_count-1).

<a name="get_nodes-1"></a>

### get_nodes/1 ###

<pre><code>
get_nodes(Ring::<a href="#type-ring">ring()</a>) -&gt; [<a href="hash_ring.md#type-ring_node">hash_ring:ring_node()</a>]
</code></pre>
<br />

ノード一覧を取得する

<a name="is_ring-1"></a>

### is_ring/1 ###

<pre><code>
is_ring(Hash_ring_dynamic::<a href="#type-ring">ring()</a> | term()) -&gt; boolean()
</code></pre>
<br />

引数の値が適切に生成されたリングオブジェクトかどうかを判定する

<a name="make-2"></a>

### make/2 ###

<pre><code>
make(Nodes::[<a href="hash_ring.md#type-ring_node">hash_ring:ring_node()</a>], Options::[<a href="#type-option">option()</a>]) -&gt; <a href="#type-ring">ring()</a>
</code></pre>
<br />

コンシステントハッシュリングを構築する

<a name="remove_nodes-2"></a>

### remove_nodes/2 ###

<pre><code>
remove_nodes(Keys::[<a href="hash_ring_node.md#type-key">hash_ring_node:key()</a>], Ring::<a href="#type-ring">ring()</a>) -&gt; <a href="#type-ring">ring()</a>
</code></pre>
<br />

ノード群を削除する

