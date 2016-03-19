

# Module hash_ring_static #
* [Description](#description)

Tuple based `hash_ring` Implementation Module.

Copyright (c) 2013-2016 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`hash_ring`](hash_ring.md).

<a name="description"></a>

## Description ##

This module represents a ring (i.e., virtual nodes) as a large tuple.

It is superior in terms of node search efficiency and memory footprint.
But dynamic addition and removal of nodes require O(N log N) and O(N) time respectively
(where N is the number of nodes in the ring).

See [README.md](../README.md#Benchmark) for a benchmark result.
