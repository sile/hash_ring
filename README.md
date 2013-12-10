hash_ring
=========
Erlangによるコンシステントハッシュ法を使ったノードリングの実装。  
試験実装レベル。

使用例
------
```erlang
%% ハッシュリングの作成
> Ring = hash_ring:make([a, b, c, d, e]).

%% アイテムを起点に、ノードを畳み込む
> hash_ring:fold(fun (Node, Acc) -> {true, [Node | Acc]} end, item_1, [], Ring).
[a,b,d,e,c]

> hash_ring:fold(fun (Node, Acc) -> {true, [Node | Acc]} end, item_2, [], Ring).
[e,b,a,c,d]
```

参考URLメモ
----------
- http://thor.cs.ucsb.edu/~ravenben/papers/coreos/kll+97.pdf
