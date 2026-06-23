# Measure directed citation influence between research groups

*Experimental.* Lifts the internal citations of a
[`sniff_groups()`](https://roneyfraga.com/birddog/reference/sniff_groups.md)
network to the group level and measures how much each group's output
flows into every other group's. Influence is **directional**: if group
\\B\\'s papers cite group \\A\\'s papers, knowledge flows \\A \to B\\.
The function returns the cross-citation matrix, four size-corrected
indices per ordered pair, the net flow between pairs, and a source /
broker / sink role per group.

## Usage

``` r
sniff_groups_influence(groups, self = TRUE, null_reps = 0, seed = NULL)

is_influence(x)

# S3 method for class 'birddog_influence'
print(x, ...)
```

## Arguments

- groups:

  A
  [`sniff_groups()`](https://roneyfraga.com/birddog/reference/sniff_groups.md)
  object: a list with a `network` component, a directed
  `tidygraph`/`igraph` whose nodes carry a `group` attribute and whose
  edges are the internal citations (citing -\> cited).

- self:

  Keep the diagonal (intra-group citations) in the matrix and in the
  out/in strengths. Default `TRUE`. `FALSE` studies only between-group
  flow.

- null_reps:

  Number of group-label permutations for a per-channel p-value: shuffle
  the group labels over documents, recompute the cross-citation matrix,
  and record how often the random flow meets or beats the observed
  count. Default `0` (no p-values). When `> 0`, `flow` gains a `p_value`
  column.

- seed:

  Optional integer seed for the permutation null, for reproducibility.
  Default `NULL`.

- x:

  A `birddog_influence` object.

- ...:

  Ignored.

## Value

An object of class `birddog_influence`, a list with:

- `matrix`: the \\G \times G\\ cross-citation matrix \\C\\ (rows cite
  columns), groups in
  [`mixed_sort()`](https://roneyfraga.com/birddog/reference/mixed_sort.md)
  order.

- `flow`: a tibble, one row per observed ordered pair, sorted by
  descending `surprise` – `influencer` (cited group), `recipient`
  (citing group), `citations`, `debt`, `audience`, `salton`, `surprise`
  (and `p_value` when `null_reps > 0`).

- `groups`: per-group `received`, `made`, `balance`, `role` (source /
  broker / sink), sorted by descending `balance`.

- `net`: the net-influence edge list – `from` (source), `to`
  (recipient), `net` (\\\nu\_{ij} \> 0\\).

- `params`: the call settings.

## Details

Writing \\C\_{ij}\\ for the number of citations from group \\i\\ to
group \\j\\ (rows cite columns), out-strength \\o_i=\sum_k C\_{ik}\\,
in-strength \\\iota_j=\sum_k C\_{kj}\\, and total
\\m=\sum\_{ij}C\_{ij}\\, each ordered pair carries four normalizations:

- **debt** \\C\_{ij}/o_i\\ – the share of \\i\\'s citations owed to
  \\j\\ (how much \\i\\ leans on \\j\\);

- **audience** \\C\_{ij}/\iota_j\\ – the share of \\j\\'s citations
  coming from \\i\\;

- **salton** \\C\_{ij}/\sqrt{o_i\\\iota_j}\\ – a size-free channel
  strength, symmetric in the pair;

- **surprise** \\C\_{ij}/(o_i\\\iota_j/m)\\ – the flow against the
  configuration-model expectation; \\\>1\\ over-represented, \\\<1\\
  under-represented.

The **net influence** \\\nu\_{ij}=C\_{ij}-C\_{ji}\\ removes reciprocal
flow, and the group **balance** \\\beta_i=\iota_i-o_i\\ (received minus
made) classifies each group as a *source* (\\\beta\>0\\, foundational),
a *sink* (\\\beta\<0\\, frontier consumer), or a *broker*
(\\\beta\approx 0\\). The balances of a closed system sum to zero. The
diagonal \\C\_{ii}\\ is intra-group cohesion; `self = FALSE` drops it
before any normalization (the balances and net are unchanged, since the
diagonal cancels).

## See also

[`sniff_groups()`](https://roneyfraga.com/birddog/reference/sniff_groups.md),
[`sniff_groups_hubs()`](https://roneyfraga.com/birddog/reference/sniff_groups_hubs.md),
[`plot_groups_influence_matrix()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_matrix.md),
[`plot_groups_influence_network()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_network.md)

Other groups (stock):
[`sniff_components()`](https://roneyfraga.com/birddog/reference/sniff_components.md),
[`sniff_groups()`](https://roneyfraga.com/birddog/reference/sniff_groups.md),
[`sniff_groups_attributes()`](https://roneyfraga.com/birddog/reference/sniff_groups_attributes.md),
[`sniff_groups_cumulative()`](https://roneyfraga.com/birddog/reference/sniff_groups_cumulative.md),
[`sniff_groups_cumulative_citations()`](https://roneyfraga.com/birddog/reference/sniff_groups_cumulative_citations.md),
[`sniff_groups_hubs()`](https://roneyfraga.com/birddog/reference/sniff_groups_hubs.md),
[`sniff_groups_lineage()`](https://roneyfraga.com/birddog/reference/sniff_groups_lineage.md),
[`sniff_network()`](https://roneyfraga.com/birddog/reference/sniff_network.md)

## Examples

``` r
if (FALSE) { # \dontrun{
groups <- sniff_groups(net)
infl <- sniff_groups_influence(groups)
infl
infl$groups          # source / broker / sink per group
infl$net             # who, on balance, leads whom
plot_groups_influence_matrix(infl)
} # }
```
