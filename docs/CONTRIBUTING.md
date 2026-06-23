# Contributing to birddog

Contributions are welcome. All new code must follow the API grammar
below, fixed at the 2026-06-12 design.

## API grammar

1.  **Three public verbs only:** `read_*` (ingest), `sniff_*`
    (detect/measure), `plot_*` (visualize). Helpers: `is_*`,
    `validate_*`. No `detect_`, `filter_`, `get_` in new exports.

2.  **Two noun families:** `sniff_groups_*` operates on stock (groups at
    a point in time); `sniff_trajectory_*` operates on flow (temporal
    objects). The word `trajectory` is always singular in function
    names. The `tr::`/`trN` label grammar belongs exclusively to the
    flow partition. Stock plots are always `plot_groups_*` (the singular
    `plot_group_*` prefix is retired).

3.  **Pipeline-object contract:** the first argument of every function
    is the object produced by the previous pipeline stage. Every
    `sniff_trajectory_*` analysis function takes the flow object. Every
    `plot_*` takes the output of its matching `sniff_*` and does not
    recompute it (the one convenience exception, the
    `plot_trajectory_lines_*` family, is named in rule 4).

4.  **Flexible entry, strict core:** the two entry points of the
    trajectory layer,
    [`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md)
    and
    [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md),
    accept any of the lineage list
    ([`sniff_groups_lineage()`](https://roneyfraga.com/birddog/reference/sniff_groups_lineage.md)
    output), `docs_per_group`, or (for flow) a dag — resolved by
    duck-typing in one internal coercer
    [`as_trajectory_dag()`](https://roneyfraga.com/birddog/reference/as_trajectory_dag.md).
    Downstream analysis functions demand the exact flow class, with two
    deliberate conveniences:
    [`sniff_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md)
    shares that entry-point duck-typing (flow, dag, lineage, or
    `docs_per_group`), and
    [`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md)
    /
    [`plot_trajectory_lines_3d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_3d.md)
    coerce a loose input to a flow and build the confluence internally
    when `conf` is not supplied.

5.  **Algorithms are detectors, not arguments:** a future detection
    algorithm is a new `sniff_trajectory_<algo>(dag, ...)` that consumes
    a `birddog_dag` and returns an object passing
    [`validate_flow()`](https://roneyfraga.com/birddog/reference/validate_flow.md)
    (same contract: `trajectories`, `tree`, `graph`, `docs_per_group`,
    `last_year`) and inheriting the contract class:
    `class = c("birddog_<algo>", "birddog_flow", "list")`. S3 then
    dispatches every contract method
    ([`subset()`](https://rdrr.io/r/base/subset.html),
    [`print()`](https://rdrr.io/r/base/print.html), future ones) to new
    detectors for free. No `method=` dumping-ground argument. “Flow”
    names the object kind, not the algorithm (stock vs flow: groups are
    stock, trajectories are flow): every detector returns a flow — the
    temporal decomposition of the DAG into trajectories plus the
    confluence tree. The canonical detector
    [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
    carries the object’s name; its internal algorithm (mutual dominance
    today) may be improved at a major version without renaming, while
    named variants pin specific algorithms.
    [`validate_flow()`](https://roneyfraga.com/birddog/reference/validate_flow.md)
    is the contract’s authoritative checker.

6.  **Stable label grammar as public contract:** `cNgN` (group),
    `y<YYYY><cNgN>` (group-year node), `tr::<cNgN>` (central), `trN`
    (absorbed). Documented in
    [`?birddog`](https://roneyfraga.com/birddog/reference/birddog-package.md)
    and never changed within a major version.

7.  **Lifecycle discipline (from 2.0.0 onward):** 2.0.0 was the one
    pre-launch hard break, justified by the absence of users to protect.
    From 2.0.0 on the rule is strict: new exports start experimental,
    promoted to stable after one CRAN cycle; anything removed goes
    through `deprecate_warn()` -\> `deprecate_stop()` -\> deletion,
    never silently (the `lifecycle` package is added when first needed).
    Breaking changes only at major versions.

8.  **Number rules:** a noun that is the verb’s direct object takes its
    natural number
    ([`sniff_groups()`](https://roneyfraga.com/birddog/reference/sniff_groups.md)
    finds many;
    [`sniff_network()`](https://roneyfraga.com/birddog/reference/sniff_network.md)
    builds one). Layer prefixes have a fixed number: `groups_` (plural,
    the stock-layer brand) and `trajectory_` (singular, attributive —
    *trajectory flow* like *citation network*). The head noun takes its
    natural number (`_attributes`, `_hubs` plural; `_dag`, `_flow`,
    `_lineage` singular; `_dynamics` plurale tantum). The plural NEVER
    distinguishes scope: one-vs-all is an argument (`target =`,
    `n_routes =`) or a `map()`, never a trailing “s” — the 1.x
    `formation()`/`formations()` trap is banned permanently.
