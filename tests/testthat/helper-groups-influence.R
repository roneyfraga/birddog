# Toy direct-citation network from todo/birddog-groups-influence.qmd:
# three contemporaneous groups, internal citation chains, between-group flow
# both ways. Used by the group-influence tests.
toy_groups <- function() {
  docs <- data.frame(
    document_id = c("a1","c1","b1","a2","c2","b2","a3","b3","c3","c4","a4","b4"),
    group       = c("G1","G3","G2","G1","G3","G2","G1","G2","G3","G3","G1","G2"),
    stringsAsFactors = FALSE
  )
  cites <- data.frame(
    from = c("a2","a3","a4", "b2","b3","b4", "c2","c3","c4","c4",
             "a2","a3", "b1","b2","b3","b4", "b2","b4", "c2","c3","c3","c4",
             "a4", "c2","c4"),
    to   = c("a1","a2","a3", "b1","b2","b3", "c1","c2","c3","c2",
             "b1","b2", "a1","a2","a3","a3", "c2","c4", "b1","b2","b3","b3",
             "c3", "a1","a2"),
    stringsAsFactors = FALSE
  )
  net <- igraph::graph_from_data_frame(cites, vertices = docs, directed = TRUE)
  list(network = net)
}
