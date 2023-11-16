test_that("plot_tree_move runs without errors", {
  # Load the example data
  f <- system.file("extdata", "Kasparov.gz", package = "bigchess")
  con <- gzfile(f,encoding = "latin1")
  df <- read.pgn(con,quiet = TRUE,stat.moves = FALSE)
  tr <- tree_move(subset(df,W1=="e4"),"B1")

  # Test that plot_tree_move runs without errors and produces a plot
  expect_silent(plot_tree_move(tr,"1. e4 ... ?"))

  # You can also test the function with different arguments
  expect_silent(plot_tree_move(tr,"1. e4 ... ?", add.lines = FALSE))
  expect_silent(plot_tree_move(tr,"1. e4 ... ?", add.labels = FALSE))
})
