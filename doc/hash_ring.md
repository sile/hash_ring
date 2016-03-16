

# Module hash_ring #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

コンシステントハッシュリング操作用のインターフェースモジュール.

Copyright (c) 2013-2016 Takeru Ohta <phjgt308@gmail.com>

__This module defines the `hash_ring` behaviour.__<br /> Required callback functions: `make/2`, `is_ring/1`, `add_nodes/2`, `remove_nodes/2`, `get_nodes/1`, `get_node_count/1`, `fold/4`.

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
hash_ring_module() = hash_ring_static | hash_ring_dynamic
</code></pre>




### <a name="type-item">item()</a> ###


<pre><code>
item() = <a href="hash_ring_node.md#type-key">hash_ring_node:key()</a>
</code></pre>




### <a name="type-option">option()</a> ###


<pre><code>
option() = {module, <a href="#type-hash_ring_module">hash_ring_module()</a>} | <a href="hash_ring_static.md#type-option">hash_ring_static:option()</a> | <a href="hash_ring_dynamic.md#type-option">hash_ring_dynamic:option()</a>
</code></pre>




### <a name="type-ring">ring()</a> ###


__abstract datatype__: `ring(_Key, _Data)`




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

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_nodes-2">add_nodes/2</a></td><td>ノード群を追加する.</td></tr><tr><td valign="top"><a href="#collect_nodes-3">collect_nodes/3</a></td><td>指定のアイテムを担当するノードを優先順位が高い順に最大<code>N</code>個集める.</td></tr><tr><td valign="top"><a href="#find_node-2">find_node/2</a></td><td>指定のアイテムを担当するノードを検索する.</td></tr><tr><td valign="top"><a href="#fold-4">fold/4</a></td><td>アイテムの次に位置するノードから順に畳み込みを行う.</td></tr><tr><td valign="top"><a href="#get_node_count-1">get_node_count/1</a></td><td>ノードの個数を取得する.</td></tr><tr><td valign="top"><a href="#get_nodes-1">get_nodes/1</a></td><td>ノード一覧を取得する.</td></tr><tr><td valign="top"><a href="#is_ring-1">is_ring/1</a></td><td>引数の値が適切に生成されたリングオブジェクトかどうかを判定する.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Equivalent to <a href="#make-2"><tt>make(Nodes, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#make-2">make/2</a></td><td>コンシステントハッシュリングを構築する.</td></tr><tr><td valign="top"><a href="#remove_nodes-2">remove_nodes/2</a></td><td>キーに対応するノード群を削除する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_nodes-2"></a>

### add_nodes/2 ###

<pre><code>
add_nodes(Nodes::[<a href="#type-ring_node">ring_node()</a>], Ring::<a href="#type-ring">ring()</a>) -&gt; <a href="#type-ring">ring()</a>
</code></pre>
<br />

ノード群を追加する

キーが等しいノードが既に存在する場合は、上書きされる

<a name="collect_nodes-3"></a>

### collect_nodes/3 ###

<pre><code>
collect_nodes(Item::<a href="#type-item">item()</a>, N::non_neg_integer(), Ring::<a href="#type-ring">ring()</a>) -&gt; [<a href="#type-ring_node">ring_node()</a>]
</code></pre>
<br />

指定のアイテムを担当するノードを優先順位が高い順に最大`N`個集める

<a name="find_node-2"></a>

### find_node/2 ###

<pre><code>
find_node(Item::<a href="#type-item">item()</a>, Ring::<a href="#type-ring">ring()</a>) -&gt; {ok, <a href="#type-ring_node">ring_node()</a>} | error
</code></pre>
<br />

指定のアイテムを担当するノードを検索する

リングが空の場合は`error`が返される

<a name="fold-4"></a>

### fold/4 ###

<pre><code>
fold(Fun::<a href="#type-fold_fun">fold_fun()</a>, Item::<a href="#type-item">item()</a>, Initial::term(), Ring::<a href="#type-ring">ring()</a>) -&gt; Result::term()
</code></pre>
<br />

アイテムの次に位置するノードから順に畳み込みを行う

<a name="get_node_count-1"></a>

### get_node_count/1 ###

<pre><code>
get_node_count(Ring::<a href="#type-ring">ring()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

ノードの個数を取得する

<a name="get_nodes-1"></a>

### get_nodes/1 ###

<pre><code>
get_nodes(Ring::<a href="#type-ring">ring()</a>) -&gt; [<a href="#type-ring_node">ring_node()</a>]
</code></pre>
<br />

ノード一覧を取得する

返り値のノードはキー順に昇順にソートされている

<a name="is_ring-1"></a>

### is_ring/1 ###

<pre><code>
is_ring(Hash_ring::<a href="#type-ring">ring()</a> | term()) -&gt; boolean()
</code></pre>
<br />

引数の値が適切に生成されたリングオブジェクトかどうかを判定する

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
make(Nodes::[<a href="#type-ring_node">ring_node()</a>], Options::[<a href="#type-option">option()</a>]) -&gt; <a href="#type-ring">ring()</a>
</code></pre>
<br />

コンシステントハッシュリングを構築する

<a name="remove_nodes-2"></a>

### remove_nodes/2 ###

<pre><code>
remove_nodes(Nodes::[<a href="hash_ring_node.md#type-key">hash_ring_node:key()</a>], Ring::<a href="#type-ring">ring()</a>) -&gt; <a href="#type-ring">ring()</a>
</code></pre>
<br />

キーに対応するノード群を削除する

