test_that('analyze_game works', {

  # If an engine is found, run the tests
  engine_path <- find_engine()
  if (!is.null(engine_path)) {
    # Set up the engine
    engine <- uci_engine(engine_path)

    # Test analyze_game with engine and SAN
    result <- suppressMessages(analyze_game(engine,
                                            san = "1. e4 e5 2. Nf3 Nc6",
                                            depth = 2))
    expect_type(result, "list")
    expect_equal(length(result), 4)
    expect_equal(result[[1]], list(curmove_lan = "e2e4", curmove_san = "e4",
                                   curpos_lan  = "e2e4", curpos_san  = "1. e4",
                                   comment = "book"))

    # Test analyze game with path and LAN
    result <- suppressMessages(analyze_game(engine_path,
                                            lan = "a2a3 a7a6 a1a2 a6a5",
                                            depth = 2))
    expect_type(result, "list")
    expect_equal(length(result), 4)
    expect_equal(result[[2]]$comment, "")
  } else {
    # Skip the tests if there is no engine available
    expect_true(is.null(engine_path))
  }

})
