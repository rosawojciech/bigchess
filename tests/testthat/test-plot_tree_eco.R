test_that("plot_tree_eco runs without errors", {
  # Load the example data
  f <- system.file("extdata", "Kasparov.gz", package = "bigchess")
  con <- gzfile(f,encoding = "latin1")
  df <- read.pgn(con,quiet = TRUE,stat.moves = FALSE, add.tags = "ECO")
  tr <- tree_eco(subset(df,W1=="e4"),20)

  # Test that plot_tree_eco runs without errors and produces a plot
  expect_silent(plot_tree_eco(tr,"1. e4 ... ?"))

  # You can also test the function with different arguments
  expect_silent(plot_tree_eco(tr,"1. e4 ... ?", add.lines = FALSE))
  expect_silent(plot_tree_eco(tr,"1. e4 ... ?", add.labels = FALSE))
})
