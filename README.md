hash_ring
=========
Erlangによるコンシステントハッシュ法を使ったノードリングの実装。  
試験実装レベル。

使用例
------
```erlang
%% ハッシュリングの作成
> Ring = hash_ring:make([a, b, c, d, e]).

%% アイテムを所有するノード一覧の取得
> hash_ring:get_owner_nodes(item_1, 2, Ring).
[a, e]

> hash_ring:get_owner_nodes(item_2, 3, Ring).
[d, c, a]
```

参考URLメモ
----------
- http://thor.cs.ucsb.edu/~ravenben/papers/coreos/kll+97.pdf
