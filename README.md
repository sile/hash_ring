hash_ring
=========

An implementation of consistent hashing algorithm.

This algorithm determines which nodes handles which items by hashing.

Consisntent hashing is suitable to manage nodes in environments in which nodes dynamically joins and leaves.

For example, if a node leaves  the cluster, the items handled by the node should be reassigned to other nodes.
But other items can remain in the current nodes.
Thus only 1/N items are affected by the leaving (where 'N' is the number of nodes in the cluster).

See [Reference](#reference) for more information.

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
%% Builds a consistent hash ring
> Nodes = hash_ring:list_to_nodes([a,b,c,d,e]).
[{hash_ring_node,a,a,1},
 {hash_ring_node,b,b,1},
 {hash_ring_node,c,c,1},
 {hash_ring_node,d,d,1},
 {hash_ring_node,e,e,1}]

> Ring0 = hash_ring:make(Nodes).

%% Finds the node which handles the item
> hash_ring:find_node(item_1, Ring0).
{ok,{hash_ring_node,c,c,1}}

%% Collects four nodes in descending order of priority
> hash_ring:collect_nodes(item_1, 4, Ring0).
[{hash_ring_node,c,c,1},
 {hash_ring_node,e,e,1},
 {hash_ring_node,b,b,1},
 {hash_ring_node,d,d,1}]

%% Addition of a node
> Ring1 = hash_ring:add_node(hash_ring_node:make(g), Ring0).
> hash_ring:collect_nodes(item_1, 4, Ring1).
[{hash_ring_node,c,c,1},
 {hash_ring_node,e,e,1},
 {hash_ring_node,b,b,1},
 {hash_ring_node,g,g,1}] % The fourth node is changed from 'd' to 'g'

%% Removal of a node
> Ring2 = hash_ring:remove_node(c, Ring1).
> hash_ring:collect_nodes(item_1, 4, Ring2).
[{hash_ring_node,e,e,1}, % 'c' is removed but The remaining order is unchanged
 {hash_ring_node,b,b,1},
 {hash_ring_node,g,g,1},
 {hash_ring_node,d,d,1}]
```

API
---

See [EDoc Documents](doc/README.md)

Reference
---------

- https://en.wikipedia.org/wiki/Consistent_hashing
- [Consistent Hashing and Random Trees: Distributed Caching Protocols for Relieving Hot Spots on the World Wide Web](https://www.akamai.com/us/en/multimedia/documents/technical-publication/consistent-hashing-and-random-trees-distributed-caching-protocols-for-relieving-hot-spots-on-the-world-wide-web-technical-publication.pdf)

Benchmark
---------

A simple benchmark result for a 500 nodes ring.

### Environment

- OS: Ubuntu 15.10
- CPU: Intel(R) Core(TM) i7-6600U CPU @ 2.60GHz
- Erlang/OTP: OTP18.2 ([package](https://packages.erlang-solutions.com/erlang/esl-erlang/FLAVOUR_1_general/esl-erlang_18.2-1~ubuntu~wily_amd64.deb))

### Result

A result of `hash_ring_bench:bench(500, [])`.

#### Toal Time and Heap Size

| module            | make_time | add_time | remove_time | find_time | heap_size |
|:------------------|----------:|---------:|------------:|----------:|----------:|
| hash_ring_static  |    178 ms |  7166 ms |     2162 ms |  0.722 ms |   1406 KB |
| hash_ring_dynamic |    396 ms |   381 ms |      367 ms |  1.141 ms |   6191 KB |

#### Average Time per Node (or Item)

| module            | add_time  | remove_time | find_time  |
|:------------------|----------:|------------:|-----------:|
| hash_ring_static  | 14.332 ms |    4.323 ms | 0.00144 ms |
| hash_ring_dynamic |  0.762 ms |    0.734 ms | 0.00228 ms |

Licence
-------

This library is released under the MIT License.
See the [COPYING](COPYING) file for full license information.
