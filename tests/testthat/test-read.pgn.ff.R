test_that("read.pgn.ff works", {

  # Load the required packages
  suppressMessages(require(ff))
  suppressMessages(require(ffbase))

  # Load the example data
  f <- system.file("extdata", "Carlsen.gz", package = "bigchess")
  con <- gzfile(f, "rbt", encoding = "latin1")

  # Test that read.pgn.ff runs without errors and returns an ffdf object
  suppressMessages(
    fdf <- read.pgn.ff(con, stat.moves = FALSE)
  )
  expect_silent(fdf)
  expect_true(is(fdf, "ffdf"))
  con <- gzfile(f, "rbt", encoding = "latin1")
  suppressMessages(
    fdf <- read.pgn.ff(con, ignore.other.games = TRUE, batch.size = 10^4)
  )
  expect_silent(fdf)
  expect_true(is(fdf, "ffdf"))

  # Clean up
  ff::delete(fdf)

})
