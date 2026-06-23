# Package index

## Ingest

- [`read_openalex()`](https://roneyfraga.com/birddog/reference/read_openalex.md)
  : Read and Process OpenAlex data
- [`read_wos()`](https://roneyfraga.com/birddog/reference/read_wos.md) :
  Read Web of Science exported files
- [`get_openalex_fields()`](https://roneyfraga.com/birddog/reference/get_openalex_fields.md)
  : Get Fields from OpenAlex for Work IDs

## Network and groups (stock)

- [`sniff_network()`](https://roneyfraga.com/birddog/reference/sniff_network.md)
  : Create Citation Networks from Bibliographic Data
- [`sniff_components()`](https://roneyfraga.com/birddog/reference/sniff_components.md)
  : Identify and Analyze Network Components
- [`sniff_groups()`](https://roneyfraga.com/birddog/reference/sniff_groups.md)
  : Detect and analyze groups in a scientific network
- [`sniff_groups_cumulative()`](https://roneyfraga.com/birddog/reference/sniff_groups_cumulative.md)
  : Analyze Cumulative Network Groups Over Time
- [`sniff_groups_lineage()`](https://roneyfraga.com/birddog/reference/sniff_groups_lineage.md)
  : Per-group lineage data
- [`sniff_groups_attributes()`](https://roneyfraga.com/birddog/reference/sniff_groups_attributes.md)
  : Calculate and Visualize Group Attributes from Scientific Networks
- [`sniff_groups_cumulative_citations()`](https://roneyfraga.com/birddog/reference/sniff_groups_cumulative_citations.md)
  : Calculate Cumulative Citations by Group and Year
- [`sniff_groups_hubs()`](https://roneyfraga.com/birddog/reference/sniff_groups_hubs.md)
  : Identify Hub Papers in Research Groups
- [`sniff_groups_influence()`](https://roneyfraga.com/birddog/reference/sniff_groups_influence.md)
  [`is_influence()`](https://roneyfraga.com/birddog/reference/sniff_groups_influence.md)
  [`print(`*`<birddog_influence>`*`)`](https://roneyfraga.com/birddog/reference/sniff_groups_influence.md)
  : Measure directed citation influence between research groups

## Group content

- [`sniff_groups_keywords()`](https://roneyfraga.com/birddog/reference/sniff_groups_keywords.md)
  : Extract representative keywords from grouped nodes
- [`sniff_groups_terms()`](https://roneyfraga.com/birddog/reference/sniff_groups_terms.md)
  : Extract and Analyze Key Terms from Research Groups
- [`sniff_groups_stm_prepare()`](https://roneyfraga.com/birddog/reference/sniff_groups_stm_prepare.md)
  : Prepare Text Data and Analyze Topic Models
- [`sniff_groups_stm_run()`](https://roneyfraga.com/birddog/reference/sniff_groups_stm_run.md)
  : Run Structural Topic Modeling Analysis

## Indicators

- [`sniff_cct()`](https://roneyfraga.com/birddog/reference/sniff_cct.md)
  : Calculate Citation Cycle Time (CCT) indicator
- [`sniff_entropy()`](https://roneyfraga.com/birddog/reference/sniff_entropy.md)
  : Calculate Entropy Based on Keywords Over Time
- [`sniff_key_route()`](https://roneyfraga.com/birddog/reference/sniff_key_route.md)
  : Identify Key Routes in Citation Networks
- [`data_state_thresholds()`](https://roneyfraga.com/birddog/reference/data_state_thresholds.md)
  : Data-driven, outlier-robust dynamic-state thresholds
- [`fixed_state_thresholds()`](https://roneyfraga.com/birddog/reference/fixed_state_thresholds.md)
  : Fixed-constant thresholds for dynamic-state classification

## Trajectory detection (flow)

- [`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md)
  : Build the soft cumulative-clustering trajectory DAG
- [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
  : Braid: the canonical detector of trajectories (central + absorbed)
- [`sniff_trajectory_channel()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_channel.md)
  : Detect channel trajectories by global optimal-path routing
- [`is_flow()`](https://roneyfraga.com/birddog/reference/is_flow.md) :
  Is this a trajectory flow object?
- [`validate_flow()`](https://roneyfraga.com/birddog/reference/validate_flow.md)
  : Validate the trajectory flow contract
- [`subset(`*`<birddog_flow>`*`)`](https://roneyfraga.com/birddog/reference/subset.birddog_flow.md)
  : Subset a trajectory flow, preserving the contract
- [`sniff_trajectory_coherence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_coherence.md)
  : Appraise the content coherence of a trajectory flow
- [`sniff_trajectory_comparison()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_comparison.md)
  : Compare trajectory detectors by content coherence

## Trajectory analysis

- [`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md)
  : Dynamic-state indicators and classification for flow trajectories
- [`sniff_trajectory_cct()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_cct.md)
  : Per-year citation cycle time (renewal pace) along each flow
  trajectory
- [`sniff_trajectory_entropy()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_entropy.md)
  : Per-year keyword diversity (Pielou's J') along each flow trajectory
- [`sniff_trajectory_hubs()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_hubs.md)
  : Hub roles aggregated to each flow trajectory (provincial vs
  bridging)
- [`sniff_trajectory_community()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_community.md)
  : Community breadth (distinct authors) of each flow trajectory
- [`sniff_trajectory_emergence_owners()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_emergence_owners.md)
  : Roll trajectory emergence up to the authors who own it
- [`sniff_trajectory_self_sufficiency()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_self_sufficiency.md)
  : Self-sufficiency of each central trajectory
- [`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md)
  : Where a trajectory's terminal cohort goes
- [`sniff_trajectory_formation()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_formation.md)
  : Trajectories that fed into a target (confluence)
- [`sniff_trajectory_contribution()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_contribution.md)
  : Documents an intermediate trajectory contributes to a target in a
  given year
- [`sniff_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md)
  : Build the render-ready confluence forest of the soft DAG

## Visualization

- [`plot_groups_influence_matrix()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_matrix.md)
  : Plot the directed group-influence matrix (heatmap)
- [`plot_groups_influence_network()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_network.md)
  : Plot the group-influence network (the net-influence spine)
- [`plot_groups_lineage_2d()`](https://roneyfraga.com/birddog/reference/plot_groups_lineage_2d.md)
  : Visualize 2D Technological Trajectories from Group Evolution
- [`plot_groups_lineage_3d()`](https://roneyfraga.com/birddog/reference/plot_groups_lineage_3d.md)
  : Visualize 3D Technological Trajectories from Group Evolution
- [`plot_groups_map()`](https://roneyfraga.com/birddog/reference/plot_groups_map.md)
  : Plot the stock map: the complete citation network coloured by final
  group
- [`plot_groups_map_animation()`](https://roneyfraga.com/birddog/reference/plot_groups_map_animation.md)
  : Animate the stock map: the citation network forming year by year
- [`plot_groups_map_interactive()`](https://roneyfraga.com/birddog/reference/plot_groups_map_interactive.md)
  : Interactive stock map: the citation network as a plotly HTML widget
- [`plot_groups_per_year()`](https://roneyfraga.com/birddog/reference/plot_groups_per_year.md)
  : Plot groups per year: the clustering fan converging to the final
  groups
- [`plot_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence.md)
  : Plot the trajectory confluence (how the central trajectories form)
- [`plot_trajectory_confluence_interactive()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence_interactive.md)
  : Interactive trajectory confluence (plotly): hover a stream for its
  trajectory
- [`plot_trajectory_confluence_matrix()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence_matrix.md)
  : Plot the trajectory confluence matrix (finals vs intermediates)
- [`plot_trajectory_dag()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag.md)
  : Plot the soft trajectory DAG (all intermediate nodes)
- [`plot_trajectory_dag_interactive()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag_interactive.md)
  : Interactive trajectory DAG (plotly): hover a node for its trajectory
- [`plot_trajectory_dispersion()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dispersion.md)
  : Plot where a stagnant trajectory's cohort dispersed (its dispersion
  fan)
- [`plot_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dynamics.md)
  : Strategic map of trajectory dynamic states
- [`plot_trajectory_dynamics_interactive()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dynamics_interactive.md)
  : Interactive strategic map of trajectory dynamic states (plotly)
- [`plot_trajectory_formation()`](https://roneyfraga.com/birddog/reference/plot_trajectory_formation.md)
  : Plot the trajectories that fed into a target (confluence timeline)
- [`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md)
  : Plot the global trajectories as 2D variable-width lines
- [`plot_trajectory_lines_3d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_3d.md)
  : Plot the global trajectories as 3D variable-width lines

## Utilities

- [`mixed_sort()`](https://roneyfraga.com/birddog/reference/mixed_sort.md)
  : Natural sort for alphanumeric strings
- [`birddog`](https://roneyfraga.com/birddog/reference/birddog-package.md)
  [`birddog-package`](https://roneyfraga.com/birddog/reference/birddog-package.md)
  : birddog: sniffing emergence and trajectories in academic papers and
  patents
