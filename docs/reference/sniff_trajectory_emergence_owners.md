# Roll trajectory emergence up to the authors who own it

The actor-level companion to
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md),
after the player scoring of Garner et al. (2017): it credits each author
with the emergence of the living trajectories their documents belong to.
An author who publishes many documents inside high-`emergence_index`
centrals is an owner of the field's emergence; one whose work sits in
mature or absorbed lineages is not.

## Usage

``` r
sniff_trajectory_emergence_owners(
  flow,
  dynamics,
  authors,
  min_docs = 1,
  by_trajectory = FALSE,
  top_n = NULL
)
```

## Arguments

- flow:

  A
  [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
  object.

- dynamics:

  A
  [`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md)
  tibble (supplies `traj_id` and `emergence_index`).

- authors:

  A long data frame with columns `document_id` and `author`, one row per
  document-author pair.

- min_docs:

  Drop authors with fewer than this many credited documents: summed
  across trajectories when aggregated, or within the trajectory when
  `by_trajectory = TRUE`. Default 1 keeps all. Use a higher cut to read
  `norm`/`share` without one-paper noise.

- by_trajectory:

  If `TRUE`, return the per-trajectory breakdown (one row per
  trajectory-author) instead of the field-level aggregate (one row per
  author). Default `FALSE`.

- top_n:

  Keep only the highest-scoring authors – overall (by `total`) when
  aggregated, or per trajectory (by `contribution`) when
  `by_trajectory = TRUE`. Default `NULL` keeps all.

## Value

A tibble.

When `by_trajectory = FALSE` (default): one row per author, sorted by
descending `total` – `author`, `total` (emergence ownership), `ndocs`
(credited document-trajectory incidences), `norm`
(`total / sqrt(ndocs)`).

When `by_trajectory = TRUE`: one row per trajectory-author, sorted by
trajectory then descending `contribution` – `traj_id`, `group`,
`author`, `ndocs` (the author's distinct documents in the trajectory,
\\n\_{a,t}\\), `emergence_index` (the trajectory's weight),
`contribution` (`emergence_index * ndocs`, the author's share of that
trajectory's emergence), and `share` (`ndocs` over the trajectory's
distinct documents – the author's coverage of its papers).

## Details

For every central trajectory with a defined `emergence_index` (the
living population), each author is credited `emergence_index(t)` per
distinct document they hold in `t`. Absorbed lineages carry no
`emergence_index` and are skipped, so a contributor to a dead tributary
is never scored as owning emergence.

\$\$Total(a) = \sum_t emergence\\index(t)\\ n\_{a,t}, \qquad Norm(a) =
Total(a) / \sqrt{\sum_t n\_{a,t}}\$\$

where \\n\_{a,t}\\ is author \\a\\'s distinct documents in trajectory
\\t\\. `Total` favours prolific owners; `Norm` (per square-root of
output, Garner's normalization) surfaces high-intensity authors with
fewer papers.

With `by_trajectory = TRUE` the per-trajectory term \\n\_{a,t}\\ is
returned instead of being summed away: one row per (trajectory, author),
so you can read which authors *dominate each living trajectory* rather
than the field as a whole. `top_n` then trims to the leading authors per
trajectory.

## See also

[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md),
[`sniff_trajectory_community()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_community.md),
[`sniff_trajectory_contribution()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_contribution.md)

Other trajectory analysis:
[`sniff_trajectory_cct()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_cct.md),
[`sniff_trajectory_community()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_community.md),
[`sniff_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md),
[`sniff_trajectory_contribution()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_contribution.md),
[`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md),
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md),
[`sniff_trajectory_entropy()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_entropy.md),
[`sniff_trajectory_formation()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_formation.md),
[`sniff_trajectory_hubs()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_hubs.md),
[`sniff_trajectory_self_sufficiency()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_self_sufficiency.md)
