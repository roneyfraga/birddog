# birddog: sniffing emergence and trajectories in academic papers and patents

Tools to detect emergence and trace technological/scientific
trajectories in papers and patents. It reads OpenAlex and Web of Science
data, builds citation-based networks, identifies groups, and summarizes
their dynamics.

## Links

- Website: <http://roneyfraga.com/birddog/>

- GitHub: <https://github.com/roneyfraga/birddog>

- Issues: <https://github.com/roneyfraga/birddog/issues>

## Pipeline

[`read_openalex()`](https://roneyfraga.com/birddog/reference/read_openalex.md)/[`read_wos()`](https://roneyfraga.com/birddog/reference/read_wos.md)
-\>
[`sniff_network()`](https://roneyfraga.com/birddog/reference/sniff_network.md)
-\>
[`sniff_components()`](https://roneyfraga.com/birddog/reference/sniff_components.md)
-\>
[`sniff_groups_cumulative()`](https://roneyfraga.com/birddog/reference/sniff_groups_cumulative.md)
-\>
[`sniff_groups_lineage()`](https://roneyfraga.com/birddog/reference/sniff_groups_lineage.md)
-\>
[`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md)
-\>
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
-\> analysis
([`sniff_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md),
`_formation()`, `_destination()`, `_contribution()`,
`_self_sufficiency()`, `_cct()`, `_entropy()`, `_hubs()`,
`_dynamics()`), each paired with a `plot_*` function. Focus an analysis
with `subset(flow, ...)`. "Flow" names the object kind, not an algorithm
(stock vs flow: groups are stock, trajectories are flow): every detector
returns a flow, and alternative detectors (`sniff_trajectory_<algo>()`)
return the same contract, checked by
[`validate_flow()`](https://roneyfraga.com/birddog/reference/validate_flow.md).

## Label grammar

`cNgN` is a group (component N, group N); `y<YYYY><cNgN>` is a
group-year node; `tr::<cNgN>` is a central trajectory (reaches the last
year, one per final group); `tr1..trN` are absorbed trajectories. This
grammar is part of the public API and only changes at major versions.

## Theoretical background

Trajectories are detected as system-level objects (Dosi, 1982): disjoint
chains in the temporal DAG of cumulative clusterings. A trajectory that
stops being detected loses its identity, not its papers; tracking its
terminal cohort to the last year classifies the outcome as convergence
(one destination), divergence (many), or dormancy (none) — with
emergence indicators on the living chains (Rotolo et al., 2015; Carley
et al., 2017).

## See also

Useful links:

- Report bugs at <https://github.com/roneyfraga/birddog/issues>

## Author

**Maintainer**: Roney Fraga Souza <roneyfraga@gmail.com>
([ORCID](https://orcid.org/orcid.org/0000-0001-5750-489X)) \[copyright
holder\]

Other contributors:

- Luis Felipe de Souza Rodrigues <lfsouza25@gmail.com> \[contributor\]
