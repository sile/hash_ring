hash_ring
=========
Erlangによるコンシステントハッシュ法を使ったノードリングの実装。

使用例
------
```erlang
%% ハッシュリングの作成
> Nodes = lists:map(fun hash_ring_node:make/1, [a,b,c,d,e]).
[{hash_ring_node,a,a,1},
 {hash_ring_node,b,b,1},
 {hash_ring_node,c,c,1},
 {hash_ring_node,d,d,1},
 {hash_ring_node,e,e,1}]

> Ring = hash_ring:make(Nodes).

%% アイテムを担当するノードを検索する
> hash_ring:find_node(item_1, Ring).
{ok,{hash_ring_node,c,c,1}}

%% アイテムを起点に、ノードを畳み込む
> hash_ring:fold(fun (Node, Acc) -> {true, Acc ++ [hash_ring_node:get_key(Node)]} end, item_1, [], Ring).
[c,e,d,b,a]  % 優先度が高い順に操作される

> hash_ring:fold(fun (Node, Acc) -> {true, Acc ++ [hash_ring_node:get_key(Node)]} end, item_2, [], Ring).
[d,c,a,b,e]
```

参考URLメモ
----------
- http://thor.cs.ucsb.edu/~ravenben/papers/coreos/kll+97.pdf
