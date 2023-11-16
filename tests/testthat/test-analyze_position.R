test_that("analyze_position works", {

  # If an engine is found, run the tests
  engine_path <- find_engine()
  if (!is.null(engine_path)) {
    # Set up the engine
    engine <- uci_engine(engine_path)

    # Test analyze_position with engine and SAN
    result <- suppressMessages(analyze_position(engine,
                                                san = '1. e4 e5 2. Nf3 Nc6',
                                                depth = 2))

    expect_type(result, 'list')
    expect_equal(result$comment, '')
    expect_equal(result$curpos_san, '1. e4 e5 2. Nf3 Nc6')

    # Test analyze_position with engine path and SAN
    result <- analyze_position(engine_path, san = '1. e4', depth = 1)
    expect_type(result, 'list')
    expect_equal(result$curpos_lan, 'e2e4')
    expect_equal(result$curpos_san, '1. e4')
    expect_equal(names(result),
                 c("bestmove_lan", "score", "bestline_lan", "curpos_lan",
                   "curpos_san", "bestline_san", "bestmove_san", "comment"))
  } else {
    # Skip the tests if there is no engine available
    expect_true(is.null(engine_path))
  }

})
