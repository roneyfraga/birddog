# Filter and rank detected trajectories

Applies post-processing filters and ranking to trajectory data based on
score, length, and other criteria. This function helps refine the output
from
[`detect_main_trajectories()`](https://roneyfraga.com/birddog/reference/detect_main_trajectories.md)
by keeping only the most relevant trajectories according to
user-specified constraints.

## Usage

``` r
filter_trajectories(tr_tbl, top_n = 3, min_score = NULL, min_length = NULL)
```

## Arguments

- tr_tbl:

  A tibble of trajectories from
  `detect_main_trajectories()$trajectories`. Must contain at least
  `traj_id`, `score`, and `length` columns.

- top_n:

  Maximum number of trajectories to keep after filtering and sorting
  (default: 3). If `NULL`, keeps all trajectories that meet the filter
  criteria.

- min_score:

  Minimum score threshold for trajectories (default: `NULL`).
  Trajectories with score less than `min_score` are discarded. Useful
  for removing weak or noisy paths.

- min_length:

  Minimum trajectory length in distinct years (default: `NULL`).
  Trajectories shorter than `min_length` are discarded. Ensures only
  trajectories spanning a meaningful temporal horizon are kept.

## Value

A filtered and sorted trajectory tibble with the same structure as
input, containing only trajectories that meet all criteria, sorted by
descending score. Returns an empty tibble if no trajectories meet the
criteria.

## Details

This function provides a straightforward way to refine trajectory
detection results by applying quality filters and ranking. The filtering
process occurs in three steps:

1.  **Quality Filtering**: Remove trajectories that don't meet minimum
    quality standards

    - `min_score`: Filters by the dynamic programming path score (higher
      = better)

    - `min_length`: Filters by temporal span in distinct years

2.  **Ranking**: Sort remaining trajectories by descending score to
    prioritize the most significant paths

3.  **Selection**: Keep only the top `top_n` trajectories after
    filtering and sorting

### Typical Use Cases

- **Focus on strongest signals**: Use `min_score` to remove
  low-confidence trajectories

- **Ensure temporal significance**: Use `min_length` to require
  multi-year evolution

- **Limit visualization complexity**: Use `top_n` to focus on the most
  important paths

- **Progressive refinement**: Chain multiple calls with different
  criteria

## See also

[`detect_main_trajectories()`](https://roneyfraga.com/birddog/reference/detect_main_trajectories.md)
for generating the trajectory data,
[`plot_group_trajectories_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_group_trajectories_lines_2d.md)
and
[`plot_group_trajectories_lines_3d()`](https://roneyfraga.com/birddog/reference/plot_group_trajectories_lines_3d.md)
for visualizing filtered trajectories

## Examples

``` r
if (FALSE) { # \dontrun{
# Get trajectories first
traj_data <- detect_main_trajectories(
  groups_cumulative_trajectories = my_data,
  group = "component1_g01"
)

# Basic: Keep top 3 trajectories by score
top_trajectories <- filter_trajectories(traj_data$trajectories)

# Keep top 5 trajectories with minimum quality standards
quality_trajectories <- filter_trajectories(
  tr_tbl = traj_data$trajectories,
  top_n = 5,
  min_score = 10,
  min_length = 4
)

# Keep all trajectories meeting minimum length (no top_n limit)
long_trajectories <- filter_trajectories(
  tr_tbl = traj_data$trajectories,
  top_n = NULL,
  min_length = 5
)

# Very strict filtering for high-quality, long trajectories
strict_trajectories <- filter_trajectories(
  tr_tbl = traj_data$trajectories,
  top_n = 3,
  min_score = 15,
  min_length = 6
)

# Use filtered trajectories for visualization
plot_group_trajectories_lines_2d(
  traj_data = traj_data,
  traj_filtered = quality_trajectories
)
} # }
```
