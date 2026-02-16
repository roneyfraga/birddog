# helper.R — Factory functions for synthetic test data
# No external files or API calls needed.

# ---------------------------------------------------------------------------
# make_minimal_openalex_dataframe()
# 6-row tibble mimicking read_openalex() output with 2 citation chains
# ---------------------------------------------------------------------------
make_minimal_openalex_dataframe <- function() {
  tibble::tibble(
    id_short = paste0("W", 1:6),
    TI = paste("Title", 1:6),
    AU = paste("Author", LETTERS[1:6]),
    PY = c(2018L, 2019L, 2019L, 2020L, 2020L, 2021L),
    DI = paste0("10.1234/fake.", 1:6),
    CR = c(
      NA_character_,
      "W1",
      "W1",
      "W2;W3",
      "W2;W3",
      "W4;W5"
    ),
    SO = rep("FAKE JOURNAL", 6),
    DT = rep("article", 6),
    DE = c(
      "keyword a; keyword b",
      "keyword a; keyword c",
      "keyword b; keyword d",
      "keyword c; keyword d; keyword e",
      "keyword a; keyword e",
      "keyword d; keyword e; keyword f"
    ),
    AB = paste("Abstract for paper", 1:6),
    C1 = rep("Country X", 6),
    TC = c(10L, 8L, 6L, 4L, 3L, 1L),
    SC = rep("Computer Science", 6),
    SR = paste0("W", 1:6),
    DB = rep("openalex_api", 6)
  )
}

# ---------------------------------------------------------------------------
# make_test_tbl_graph(db_type = "wos")
# 20-node directed tbl_graph with 2 groups (c1g1, c1g2), all standard
# node attributes, WoS-format CR strings for CCT testing.
# ---------------------------------------------------------------------------
make_test_tbl_graph <- function(db_type = "wos") {
  if (db_type == "wos") {
    # 20 nodes: 10 per group, years 2015-2020 / 2016-2021
    nodes <- tibble::tibble(
      name = sprintf("101234FAKE%04d%03d", rep(2015:2020, each = 2), rep(1:2, 6)),
      SR = paste0("WOS:", sprintf("%012d", 1:12)),
      AU = paste("Author", LETTERS[1:12]),
      TI = paste("Title", 1:12),
      PY = rep(2015:2020, each = 2),
      DE = rep(c(
        "kw1; kw2", "kw2; kw3", "kw1; kw3; kw4",
        "kw4; kw5", "kw2; kw5; kw6", "kw1; kw6"
      ), each = 2),
      AB = paste("Abstract", 1:12),
      SO = rep("FAKE JOURNAL", 12),
      TC = 12:1,
      DI = sprintf("10.1234/fake.%04d.%03d", rep(2015:2020, each = 2), rep(1:2, 6)),
      DI2 = sprintf("101234FAKE%04d%03d", rep(2015:2020, each = 2), rep(1:2, 6)),
      CR = c(
        NA_character_,
        NA_character_,
        "Author A, 2015, FAKE JOURNAL, V1, P1, DOI 10.1234/fake.2015.001",
        "Author B, 2015, FAKE JOURNAL, V1, P2, DOI 10.1234/fake.2015.002",
        "Author C, 2016, FAKE JOURNAL, V2, P1, DOI 10.1234/fake.2016.001; Author D, 2016, FAKE JOURNAL, V2, P2, DOI 10.1234/fake.2016.002",
        "Author C, 2016, FAKE JOURNAL, V2, P1, DOI 10.1234/fake.2016.001",
        "Author E, 2017, FAKE JOURNAL, V3, P1, DOI 10.1234/fake.2017.001; Author F, 2017, FAKE JOURNAL, V3, P2, DOI 10.1234/fake.2017.002",
        "Author E, 2017, FAKE JOURNAL, V3, P1, DOI 10.1234/fake.2017.001",
        "Author G, 2018, FAKE JOURNAL, V4, P1, DOI 10.1234/fake.2018.001; Author H, 2018, FAKE JOURNAL, V4, P2, DOI 10.1234/fake.2018.002",
        "Author G, 2018, FAKE JOURNAL, V4, P1, DOI 10.1234/fake.2018.001",
        "Author I, 2019, FAKE JOURNAL, V5, P1, DOI 10.1234/fake.2019.001; Author J, 2019, FAKE JOURNAL, V5, P2, DOI 10.1234/fake.2019.002",
        "Author I, 2019, FAKE JOURNAL, V5, P1, DOI 10.1234/fake.2019.001"
      ),
      DB = rep("wos_bib_normalized_names", 12),
      NT = rep("direct-citation", 12),
      component = rep("c1", 12),
      group = rep(c("c1g1", "c1g2"), 6)
    )

    # Also create 8 more nodes for group c1g2 with different years
    nodes2 <- tibble::tibble(
      name = sprintf("101234FAKE%04d%03d", rep(2016:2019, each = 2), rep(3:4, 4)),
      SR = paste0("WOS:", sprintf("%012d", 13:20)),
      AU = paste("Author", LETTERS[13:20]),
      TI = paste("Title", 13:20),
      PY = rep(2016:2019, each = 2),
      DE = rep(c("kw7; kw8", "kw8; kw9", "kw7; kw9; kw10", "kw10; kw11"), each = 2),
      AB = paste("Abstract", 13:20),
      SO = rep("FAKE JOURNAL", 8),
      TC = 8:1,
      DI = sprintf("10.1234/fake.%04d.%03d", rep(2016:2019, each = 2), rep(3:4, 4)),
      DI2 = sprintf("101234FAKE%04d%03d", rep(2016:2019, each = 2), rep(3:4, 4)),
      CR = c(
        NA_character_,
        NA_character_,
        "Author M, 2016, FAKE JOURNAL, V2, P3, DOI 10.1234/fake.2016.003; Author N, 2016, FAKE JOURNAL, V2, P4, DOI 10.1234/fake.2016.004",
        "Author M, 2016, FAKE JOURNAL, V2, P3, DOI 10.1234/fake.2016.003",
        "Author O, 2017, FAKE JOURNAL, V3, P3, DOI 10.1234/fake.2017.003; Author P, 2017, FAKE JOURNAL, V3, P4, DOI 10.1234/fake.2017.004",
        "Author O, 2017, FAKE JOURNAL, V3, P3, DOI 10.1234/fake.2017.003",
        "Author Q, 2018, FAKE JOURNAL, V4, P3, DOI 10.1234/fake.2018.003; Author R, 2018, FAKE JOURNAL, V4, P4, DOI 10.1234/fake.2018.004",
        "Author Q, 2018, FAKE JOURNAL, V4, P3, DOI 10.1234/fake.2018.003"
      ),
      DB = rep("wos_bib_normalized_names", 8),
      NT = rep("direct-citation", 8),
      component = rep("c1", 8),
      group = rep("c1g2", 8)
    )

    all_nodes <- dplyr::bind_rows(nodes, nodes2)

    # Edges: newer nodes cite older nodes (within each group)
    edges <- tibble::tibble(
      from = c(
        # c1g1 edges (using row indices after we build the graph)
        all_nodes$name[3], all_nodes$name[4],
        all_nodes$name[5], all_nodes$name[5],
        all_nodes$name[6], all_nodes$name[7],
        all_nodes$name[7], all_nodes$name[8],
        all_nodes$name[9], all_nodes$name[9],
        all_nodes$name[10], all_nodes$name[10],
        all_nodes$name[11], all_nodes$name[12],
        # c1g2 edges
        all_nodes$name[15], all_nodes$name[15],
        all_nodes$name[16], all_nodes$name[17],
        all_nodes$name[17], all_nodes$name[18],
        all_nodes$name[19], all_nodes$name[19],
        all_nodes$name[20], all_nodes$name[20]
      ),
      to = c(
        # c1g1 targets
        all_nodes$name[1], all_nodes$name[2],
        all_nodes$name[3], all_nodes$name[4],
        all_nodes$name[3], all_nodes$name[5],
        all_nodes$name[6], all_nodes$name[5],
        all_nodes$name[7], all_nodes$name[8],
        all_nodes$name[9], all_nodes$name[10],
        all_nodes$name[9], all_nodes$name[10],
        # c1g2 targets
        all_nodes$name[13], all_nodes$name[14],
        all_nodes$name[14], all_nodes$name[15],
        all_nodes$name[16], all_nodes$name[15],
        all_nodes$name[17], all_nodes$name[18],
        all_nodes$name[19], all_nodes$name[20]
      )
    )

    g <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = all_nodes)
    tidygraph::as_tbl_graph(g)

  } else {
    # OpenAlex variant
    nodes <- tibble::tibble(
      name = paste0("W", 1:20),
      SR = paste0("W", 1:20),
      AU = paste("Author", LETTERS[1:20]),
      TI = paste("Title", 1:20),
      PY = rep(2018:2022, each = 4),
      DE = rep(c("kw1; kw2", "kw2; kw3", "kw3; kw4", "kw1; kw4"), 5),
      AB = paste("Abstract", 1:20),
      SO = rep("FAKE JOURNAL", 20),
      TC = 20:1,
      DI = paste0("10.1234/fake.", 1:20),
      CR = c(
        rep(NA_character_, 4),
        paste0("W", 1:4),
        paste0("W", 5:8),
        paste0("W", 9:12),
        paste0("W", 13:16)
      ),
      DB = rep("openalex_api", 20),
      NT = rep("direct-citation", 20),
      component = rep("c1", 20),
      group = rep(c("c1g1", "c1g2"), 10)
    )

    edges <- tibble::tibble(
      from = nodes$name[5:20],
      to = nodes$name[c(1:4, 5:8, 9:12, 13:16)]
    )

    g <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = nodes)
    tidygraph::as_tbl_graph(g)
  }
}

# ---------------------------------------------------------------------------
# make_sniff_groups_output(db_type = "wos")
# list(aggregate, network, pubs_by_year) matching sniff_groups() output
# ---------------------------------------------------------------------------
make_sniff_groups_output <- function(db_type = "wos") {
  net <- make_test_tbl_graph(db_type)

  node_data <- net |>
    tidygraph::activate(nodes) |>
    tibble::as_tibble()

  aggregate <- node_data |>
    dplyr::group_by(.data$group) |>
    dplyr::summarise(
      quantity_papers = dplyr::n(),
      average_age = mean(.data$PY, na.rm = TRUE),
      .groups = "drop"
    )

  pubs_by_year <- node_data |>
    dplyr::group_by(.data$group, .data$PY) |>
    dplyr::tally(name = "publications") |>
    dplyr::rename(year = .data$PY) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$year, .data$group)

  # Add aggregate columns to network nodes
  net2 <- net |>
    tidygraph::activate(nodes) |>
    dplyr::left_join(aggregate, by = "group")

  list(
    aggregate = aggregate,
    network = net2,
    pubs_by_year = pubs_by_year
  )
}

# ---------------------------------------------------------------------------
# make_sniff_components_output(db_type = "wos")
# list(components, network) matching sniff_components() output
# ---------------------------------------------------------------------------
make_sniff_components_output <- function(db_type = "wos") {
  net <- make_test_tbl_graph(db_type)

  # Remove group column so sniff_groups() can add it fresh
  net_clean <- net |>
    tidygraph::activate(nodes) |>
    dplyr::select(-dplyr::any_of("group"))

  node_data <- net_clean |>
    tidygraph::activate(nodes) |>
    tibble::as_tibble()

  components <- node_data |>
    dplyr::group_by(.data$component) |>
    dplyr::summarise(
      quantity_publications = dplyr::n(),
      average_age = mean(.data$PY, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    components = components,
    network = net_clean
  )
}

# ---------------------------------------------------------------------------
# make_simple_dag()
# 5-node linear chain DAG for deterministic key-route testing.
# A -> B -> C -> D -> E (directed, with PY increasing)
# ---------------------------------------------------------------------------
make_simple_dag <- function() {
  nodes <- tibble::tibble(
    name = c("n1", "n2", "n3", "n4", "n5"),
    AU = paste("Author", LETTERS[1:5]),
    TI = paste("Title", 1:5),
    PY = c(2015L, 2016L, 2017L, 2018L, 2019L),
    DE = rep("kw1; kw2", 5),
    AB = paste("Abstract", 1:5),
    SO = rep("FAKE JOURNAL", 5),
    TC = c(10L, 8L, 6L, 4L, 2L),
    DI = paste0("10.1234/fake.", 1:5),
    SR = paste0("WOS:", sprintf("%012d", 1:5)),
    DB = rep("wos_bib_normalized_names", 5),
    NT = rep("direct-citation", 5),
    group = rep("full_network", 5),
    component = rep("c1", 5),
    CR = c(
      NA_character_,
      "Author A, 2015, FAKE JOURNAL, V1, P1, DOI 10.1234/fake.1",
      "Author B, 2016, FAKE JOURNAL, V2, P1, DOI 10.1234/fake.2",
      "Author C, 2017, FAKE JOURNAL, V3, P1, DOI 10.1234/fake.3",
      "Author D, 2018, FAKE JOURNAL, V4, P1, DOI 10.1234/fake.4"
    )
  )

  # Edges: newer papers cite older (n2->n1, n3->n2, n4->n3, n5->n4)
  edges <- tibble::tibble(
    from = c("n2", "n3", "n4", "n5"),
    to = c("n1", "n2", "n3", "n4")
  )

  g <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = nodes)
  tidygraph::as_tbl_graph(g)
}

# ---------------------------------------------------------------------------
# make_tracked_cr_py(db_type = "wos")
# Pre-computed CR/CR_PY tibble to bypass API calls in CCT tests.
# ---------------------------------------------------------------------------
make_tracked_cr_py <- function(db_type = "wos") {
  if (db_type == "wos") {
    tibble::tibble(
      CR = c(
        "Author A, 2015, FAKE JOURNAL, V1, P1, DOI 10.1234/fake.2015.001",
        "Author B, 2015, FAKE JOURNAL, V1, P2, DOI 10.1234/fake.2015.002",
        "Author C, 2016, FAKE JOURNAL, V2, P1, DOI 10.1234/fake.2016.001",
        "Author D, 2016, FAKE JOURNAL, V2, P2, DOI 10.1234/fake.2016.002",
        "Author E, 2017, FAKE JOURNAL, V3, P1, DOI 10.1234/fake.2017.001",
        "Author F, 2017, FAKE JOURNAL, V3, P2, DOI 10.1234/fake.2017.002",
        "Author G, 2018, FAKE JOURNAL, V4, P1, DOI 10.1234/fake.2018.001",
        "Author H, 2018, FAKE JOURNAL, V4, P2, DOI 10.1234/fake.2018.002",
        "Author I, 2019, FAKE JOURNAL, V5, P1, DOI 10.1234/fake.2019.001",
        "Author J, 2019, FAKE JOURNAL, V5, P2, DOI 10.1234/fake.2019.002",
        "Author M, 2016, FAKE JOURNAL, V2, P3, DOI 10.1234/fake.2016.003",
        "Author N, 2016, FAKE JOURNAL, V2, P4, DOI 10.1234/fake.2016.004",
        "Author O, 2017, FAKE JOURNAL, V3, P3, DOI 10.1234/fake.2017.003",
        "Author P, 2017, FAKE JOURNAL, V3, P4, DOI 10.1234/fake.2017.004",
        "Author Q, 2018, FAKE JOURNAL, V4, P3, DOI 10.1234/fake.2018.003",
        "Author R, 2018, FAKE JOURNAL, V4, P4, DOI 10.1234/fake.2018.004"
      ),
      CR_PY = c(
        2015L, 2015L, 2016L, 2016L, 2017L, 2017L,
        2018L, 2018L, 2019L, 2019L, 2016L, 2016L,
        2017L, 2017L, 2018L, 2018L
      )
    )
  } else {
    tibble::tibble(
      CR = paste0("W", 1:16),
      CR_PY = rep(2018:2021, each = 4)
    )
  }
}
