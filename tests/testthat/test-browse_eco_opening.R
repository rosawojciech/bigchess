test_that("browse_eco_opening works", {
  f <- system.file("extdata", "Kasparov.gz", package = "bigchess")
  con <- gzfile(f, encoding = "latin1")
  df <- read.pgn(con, quiet = TRUE, ignore.other.games = TRUE,
                 stat.moves = FALSE, add.tags = "ECO")
  result <- browse_eco_opening(subset(df, grepl("Kasparov", White)), topn = 20)
  expect_type(result, "list")
  expect_equal(names(result), c("ECO", "White_score", "Draws_percent",
                                "Black_score", "N"))
  expect_true(nrow(result) == 20)
  result <- browse_eco_opening(subset(df, grepl("Kasparov", White)), topn = 5)
  expect_true(nrow(result) == 5)
})
