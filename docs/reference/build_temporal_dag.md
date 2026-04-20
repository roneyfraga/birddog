# Build temporal directed acyclic graph from trajectory data

Constructs a DAG from group trajectory data by filtering edges based on
Jaccard similarity and node attributes, then keeping only the strongest
outgoing connections per node.

## Usage

``` r
build_temporal_dag(
  groups_cumulative_trajectories,
  group,
  jaccard_min = 0.05,
  intra_min = 0.1,
  k_out = 2
)
```

## Arguments

- groups_cumulative_trajectories:

  List with `groups_similarity` and `groups_attributes` components

- group:

  Character ID of the group to process (e.g., "component1_g01")

- jaccard_min:

  Minimum Jaccard similarity for edges (default: 0.05)

- intra_min:

  Minimum proportion of tracked documents within group for nodes
  (default: 0.10)

- k_out:

  Maximum number of outgoing edges to keep per node (default: 2)

## Value

An igraph object representing the temporal DAG
