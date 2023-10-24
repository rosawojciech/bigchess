test_that("write.pgn writes PGN data.frame into file", {

  # Load example data
  f <- system.file("extdata", "2016_Candidates.pgn", package = "bigchess")
  suppressMessages(
    df <- read.pgn(f)
  )
  df <- droplevels(df[1:3, ]) # keep it small for test speed

  # Write the data to a new file
  write.pgn(df, file = "my_file.pgn")

  # Read the data back from the new file
  suppressMessages(
    df2 <- read.pgn("my_file.pgn")
  )

  # Check that the written and read data are equal
  expect_equal(df, df2)

  # Bug: saving with source.movetext = TRUE adds spaces to beginning and end
  suppressMessages(
    df <- read.pgn(f, source.movetext = TRUE)
  )
  write.pgn(df, file = "my_file.pgn", source.movetext = TRUE)

  # Clean up
  unlink("my_file.pgn")

})
