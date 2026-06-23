# Novelty of a trajectory: share of its documents that arrived recently

Each document's arrival year is the first year it appears in one of the
trajectory's own nodes; novelty is the share arriving within the last
`window` years of the timeline (pillar: novelty – "recent documents tend
to be emergent").

## Usage

``` r
.traj_novelty(nodes, docs_per_group, last_year, window = 5)
```
