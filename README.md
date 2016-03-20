hash_ring
=========

An implementation of consistent hashing algorithm.

This algorithm determines which nodes handles which items by hashing.

Consisntent hashing is suitable to manage nodes in environments in which nodes dynamically joins and leaves.

For example, if a node leaves  the cluster, the items handled by the node should be reassigned to other nodes.
But other items can remain in the current nodes.
Thus only 1/N items are affected by the leaving (where 'N' is the number of nodes in the cluster).

See [Reference](#Reference) for more information.

Build
-----

To build the library for stand-alone usage:
```sh
$ git clone https://github.com/sile/hash_ring.git
$ cd hash_ring
$ ./rebar3 compile
$ ./rebar3 shell
$ > hash_ring:module_info().
```

If you want to use from your application:
```erlang
%% In your 'rebar.config'

%% Add following lines
{deps,
 [
   hash_ring
 ]}.
```

Example
-------

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

API
---

See [EDoc Document](doc/README.md)

Reference
---------

- https://en.wikipedia.org/wiki/Consistent_hashing
- [Consistent Hashing and Random Trees: Distributed Caching Protocols for Relieving Hot Spots on the World Wide Web](https://www.akamai.com/us/en/multimedia/documents/technical-publication/consistent-hashing-and-random-trees-distributed-caching-protocols-for-relieving-hot-spots-on-the-world-wide-web-technical-publication.pdf)

Benchmark
---------

Licence
-------

This library is released under the MIT License.
See the [LICENSE] file for full license information.
