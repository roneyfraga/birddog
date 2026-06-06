test_that(".principal_line follows heaviest successors to the sink", {
  succ <- c(y2000c1g1 = "y2001c1g1", y2001c1g1 = "y2002c1g4")  # sink y2002c1g4
  expect_equal(.principal_line("y2000c1g1", succ),
               c("y2000c1g1", "y2001c1g1", "y2002c1g4"))
  expect_equal(.principal_line("y2002c1g4", succ), "y2002c1g4")  # already a sink
})
