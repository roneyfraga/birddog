# Detect main temporal trajectories in group-year DAG

Identifies the most significant temporal trajectories within a group's
evolution over time by building a directed acyclic graph (DAG) from
similarity data and extracting highest-scoring disjoint paths using
dynamic programming.

## Usage

``` r
detect_main_trajectories(
  groups_cumulative_trajectories,
  group,
  jaccard_min = 0.05,
  intra_min = 0.1,
  k_out = 2,
  alpha = 1,
  beta = 0.1,
  top_M = 5,
  min_len = 3,
  use_docs_per_group = TRUE
)
```

## Arguments

- groups_cumulative_trajectories:

  List containing three components:

  - `groups_similarity`: Nested list with similarity data for each
    group, containing edges with `from`, `to`, `weight` (Jaccard), and
    `documents`

  - `groups_attributes`: Nested list with node attributes for each
    group, containing `quantity_papers`, `prop_tracked_intra_group`,
    `tracked_documents`, `PY.sd`, and `network_until`

  - `docs_per_group`: Data frame mapping group IDs to document IDs for
    accurate unique document counting

- group:

  Character ID of the group to analyze (e.g., "component1_g01")

- jaccard_min:

  Minimum Jaccard similarity for edges (default: 0.05). Higher values
  create sparser graphs with stronger connections.

- intra_min:

  Minimum proportion of tracked documents within group for nodes
  (default: 0.10). Higher values filter out weaker nodes.

- k_out:

  Maximum number of outgoing edges to keep per node (default: 2).
  Controls graph sparsity - lower values create simpler backbone
  structures.

- alpha:

  Weight for edge strength in path scoring (default: 1). Higher values
  emphasize transition strength over node quality.

- beta:

  Per-step persistence bonus in path scoring (default: 0.1). Higher
  values encourage longer trajectories.

- top_M:

  Maximum number of disjoint trajectories to extract (default: 5)

- min_len:

  Minimum number of distinct years for valid trajectory (default: 3)

- use_docs_per_group:

  Whether to use document IDs for accurate unique document counting
  (default: TRUE). If FALSE, uses approximation.

## Value

A list with two components:

- `graph`: An igraph object representing the temporal DAG with scoring
  attributes and optional document IDs

- `trajectories`: A tibble of detected trajectories sorted by score,
  with columns:

  - `traj_id`: Trajectory identifier ("tr1", "tr2", ...)

  - `start`, `end`: First and last year of the trajectory

  - `length`: Number of distinct years in the trajectory

  - `nodes`: List of node names along the path (e.g., "y2009g03")

  - `score`: Total path score from dynamic programming

  - `mean_w`: Mean edge score along the path

  - `sum_docs`: Count of unique documents covered by the path

  - `mean_size`: Mean node size (quantity_papers × proportion tracked)

  - `mean_PYsd`: Mean publication year standard deviation

## Details

This function implements a comprehensive pipeline for detecting
significant temporal trajectories in research group evolution:

### Algorithm Overview

1.  **Build Temporal DAG**: Constructs a directed acyclic graph where:

    - Nodes represent group-year combinations filtered by `intra_min`
      quality threshold

    - Edges represent transitions between consecutive years filtered by
      `jaccard_min`

    - Graph is sparsified to top `k_out` edges per node

2.  **Score Components**: Computes node and edge scores:

    - Node score: \\s_v = \log(1 + \text{quantity\\papers}\_v \times
      \text{prop\\tracked\\intra\\group}\_v)\\

    - Edge score: \\s_e = \text{weight}\_e \times \log(1 +
      \text{documents}\_e)\\

3.  **Extract Trajectories**: Uses dynamic programming to find heaviest
    paths:

    - Path score: \\\text{best}(v) = \max\left( s_v, \max\_{u \to v}
      \left( \text{best}(u) + s_v + \alpha \cdot s\_{(u,v)} + \beta
      \right) \right)\\

    - Iteratively extracts top `top_M` disjoint trajectories

    - Trajectories must span at least `min_len` distinct years

4.  **Count Documents**: Calculates unique document coverage:

    - If `use_docs_per_group = TRUE`: Exact count via set union of
      document IDs

    - Otherwise: Approximation: \\\sum \text{node documents} - \sum
      \text{edge documents}\\

### Parameter Tuning Guidance

- For **smoother, longer trajectories**: Increase `beta` (persistence
  bonus)

- For **transition-focused scoring**: Increase `alpha` (edge weight)

- For **denser connectivity**: Lower `jaccard_min` or increase `k_out`

- For **higher quality nodes**: Increase `intra_min`

- For **exact document counts**: Ensure `use_docs_per_group = TRUE` and
  provide `docs_per_group` data

## See also

[`filter_trajectories()`](https://roneyfraga.com/birddog/reference/filter_trajectories.md)
for post-processing detected trajectories,
[`plot_group_trajectories_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_group_trajectories_lines_2d.md)
and
[`plot_group_trajectories_lines_3d()`](https://roneyfraga.com/birddog/reference/plot_group_trajectories_lines_3d.md)
for visualization

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with default parameters
trajectories <- detect_main_trajectories(
  groups_cumulative_trajectories = my_data,
  group = "component1_g01"
)

# Tuned for longer, transition-focused trajectories
trajectories <- detect_main_trajectories(
  groups_cumulative_trajectories = my_data,
  group = "component1_g01",
  jaccard_min = 0.03, # More permissive connectivity
  k_out = 3, # Denser backbone
  alpha = 1.5, # Emphasize edge strength
  beta = 0.2, # Encourage longer paths
  top_M = 8, # Extract more trajectories
  min_len = 4 # Require longer trajectories
)

# Access results
graph <- trajectories$graph
trajectory_data <- trajectories$trajectories

# Plot the top trajectory
top_trajectory <- trajectory_data[1, ]
} # }
```
