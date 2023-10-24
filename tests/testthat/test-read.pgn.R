test_that("read.pgn runs without errors and returns a data frame", {

  # Load the example data
  f <- system.file("extdata", "Carlsen.gz", package = "bigchess")
  con <- gzfile(f, "rbt", encoding = "latin1")

  # Test that read.pgn runs without errors and returns a data frame
  suppressMessages(
    df <- read.pgn(con, stat.moves = FALSE, extract.moves = -1,
                   ignore.other.games = TRUE)
  )
  expect_silent(df)
  expect_true(is(df, "data.frame"))

  # Test with Elo columns
  f <- system.file("extdata", "2016_Candidates.pgn", package = "bigchess")
  suppressMessages(df <- read.pgn(f))
  df$BlackElo <- rep("2600", nrow(df))
  df$WhiteElo <- rep("2500", nrow(df))
  # Write the data to a new file
  f <- file.path(system.file(package = "bigchess"), "my_file.pgn")
  write.pgn(df, file = f, add.tags = c("WhiteElo", "BlackElo"))
  # Test that read.pgn runs without errors and returns a data frame
  suppressMessages(
    df <- read.pgn(f, stat.moves = FALSE, add.tags = c("WhiteElo", "BlackElo"))
  )
  expect_silent(df)
  expect_true(is(df, "data.frame"))
  unlink(f)

  # Clean up
  rm(df)

})
