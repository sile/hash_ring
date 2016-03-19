

# Module hash_ring_dynamic #
* [Description](#description)

Balanced Tree based `hash_ring` Implementation Module.

Copyright (c) 2013-2016 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`hash_ring`](hash_ring.md).

<a name="description"></a>

## Description ##

This module represents a ring (i.e., virtual nodes) as a [General Balanced Tree](http://erlang.org/doc/man/gb_trees.html).

Dynamic addition and removal of nodes require O(log N) time.
It is superior to [`hash_ring_dynamic`](hash_ring_dynamic.md) at the expense of node search efficiency and memory footprint.

See [README.md](../README.md#Benchmark) for a benchmark result.
