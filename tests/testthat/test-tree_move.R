test_that("tree_move correctly computes move tree", {
  f <- system.file("extdata", "Kasparov.gz", package = "bigchess")
  con <- gzfile(f,encoding = "latin1")
  df <- read.pgn(con,quiet = TRUE,stat.moves = FALSE)
  result <- tree_move(subset(df,W1=="e4"),move = "B1")
  expect_type(result, "list")
  expect_true(all(colnames(result) %in% c("B1", "White_score", "Draws_percent", "Black_score", "N")))
})

test_that("tree_move handles non-standard input", {
  df <- data.frame(W1 = c("e4", "d4", "e4", "d4", "e4"), Result = c("1-0", "2-0", "0-1", "1/2-1/2", "1-0"))
  expect_error(tree_move(df, "W1"))
})
