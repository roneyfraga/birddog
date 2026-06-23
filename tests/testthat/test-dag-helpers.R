test_that(".dag_label_text maps ids and falls back to the bare id", {
  lab <- data.frame(id = c("c1g1", "c1g2"), text = c("Alpha", "Beta"),
                    stringsAsFactors = FALSE)
  expect_equal(.dag_label_text(c("c1g1", "c1g3"), lab), c("Alpha", "c1g3"))
  expect_equal(.dag_label_text(c("c1g1", "c1g2"), NULL), c("c1g1", "c1g2"))
})

test_that(".dag_label_text warns when no id matches", {
  lab <- data.frame(id = "zzz", text = "Nope", stringsAsFactors = FALSE)
  expect_warning(.dag_label_text("c1g1", lab), "match")
})

test_that(".dag_label_text rejects a malformed labels_text", {
  expect_error(.dag_label_text("c1g1", data.frame(id = "c1g1")), "labels_text")
})
