test_that("player_profile works", {

  # Load some example data
  f <- system.file("extdata", "Kasparov.gz", package = "bigchess")
  con <- gzfile(f, encoding = "latin1")
  df <- read.pgn(con, quiet = TRUE, ignore.other.games = TRUE)

  # Test player_profile
  result <- player_profile(df, "Kasparov, G.")
  expect_equal(nrow(result), 1566)
  suppressMessages(result <- player_profile(df, "Frankenstein, V"))
})
