test_that("read.pgn.db runs without errors", {

  # Load the example data
  f <- system.file("extdata", "Carlsen.gz", package = "bigchess")
  con <- gzfile(f,"rbt",encoding = "latin1")

  # Create a temporary SQLite database for testing
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")

  # Test that read.pgn.db runs without errors and produces a message
  suppressMessages(
    expect_message(read.pgn.db(con, conn = conn, batch.size = 10^4),
                   "games imported successfully")
  )

  # Check that the table was created
  expect_true("pgn" %in% RSQLite::dbListTables(conn))

  # Clean up
  RSQLite::dbDisconnect(conn)

})
