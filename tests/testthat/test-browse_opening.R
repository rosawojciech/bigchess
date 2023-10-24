test_that("browse_opening works", {

  # Load a sample data set for testing
  f <- system.file("extdata", "Kasparov.gz", package = "bigchess")
  con <- gzfile(f, encoding = "latin1")
  df <- read.pgn(con, quiet = TRUE, ignore.other.games = TRUE, stat.moves = FALSE)

  # Test the function without moves
  result <- browse_opening(df)
  expect_type(result, "list")
  expect_equal(names(result), c("W1", "White_score", "Draws_percent",
                                "Black_score", "N"))
  expect_true(nrow(result) == 9)  # Check that the result contains at least one opening

  # Test with moves
  result <- browse_opening(df, "1.e4 e5 2.Nf3 Nc6 3.Bb5")
  expect_type(result, "list")
  expect_equal(names(result), c("B3", "White_score", "Draws_percent",
                                "Black_score", "N"))
  expect_true(nrow(result) == 8)  # Check that the result contains at least one opening

  # Test with white's turn
  result <- browse_opening(df, "1.e4 e5 2.Nf3 Nc6")
  expect_type(result, "list")
  expect_equal(names(result), c("W3", "White_score", "Draws_percent",
                                "Black_score", "N"))
  expect_true(nrow(result) == 4)

})

test_that("browse_opening works with empty dataset", {

  # Create an empty data frame for testing
  df <- data.frame()

  # Test without specifying a move
  result <- suppressMessages(browse_opening(df))

  # Check that the result is an empty list
  expect_type(result, "list")
  expect_equal(length(result$W1), 0)

  # Test with a specified move
  result <- suppressMessages(browse_opening(df, "1.e4 e5 2.Nf3 Nc6 3.Bb5"))

  # Check that the result is an empty list
  expect_type(result, "list")
  expect_equal(length(result$W1), 0)

})
