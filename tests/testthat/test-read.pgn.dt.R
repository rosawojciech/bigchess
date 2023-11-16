test_that("read.pgn.dt works", {

  # Load the required packages
  suppressMessages(require(data.table))

  # Load the example data
  f <- system.file("extdata", "Carlsen.gz", package = "bigchess")
  con <- gzfile(f, "rbt", encoding = "latin1")

  # Test that read.pgn.dt runs without errors and returns a data.table object
  suppressMessages(
    dt <- read.pgn.dt(con, stat.moves = FALSE)
  )
  expect_silent(dt)
  expect_true(is(dt, "data.table"))
  con <- gzfile(f, "rbt", encoding = "latin1")
  suppressMessages(
    dt <- read.pgn.dt(con, ignore.other.games = TRUE, batch.size = 10^4)
  )
  expect_silent(dt)
  expect_true(is(dt, "data.table"))

  # Clean up
  rm(dt)

})
