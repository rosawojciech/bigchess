test_that("uci functions work", {

  # If an engine is found, run the uci tests
  expect_silent(engine_path <- find_engine())
  if (!is.null(engine_path)) {

    # Set up the engine
    # Test uci_engine
    expect_silent(engine <- uci_engine(engine_path))

    # Test uci_uci
    expect_silent(engine <- uci_uci(engine))

    # Test uci_register
    expect_silent(engine <- uci_register(engine, later = TRUE))
    expect_silent(engine <- uci_register(engine, name = 'name', code = 'code'))

    # Test uci_setoption
    expect_silent(engine <- uci_setoption(engine, name  = "Clear",
                                                  value = "Hash"))

    # Test uci_isready
    expect_silent(engine <- uci_isready(engine))

    # Test uci_cmd
    expect_silent(engine <- uci_cmd(engine, "go depth 2"))

    # Test uci_debug
    expect_silent(engine <- uci_debug(engine, TRUE))
    expect_silent(engine <- uci_debug(engine, FALSE))

    # Test uci_ucinewgame
    expect_silent(engine <- uci_ucinewgame(engine))

    # Test uci_go
    expect_silent(uci_go(engine, depth = 2,
                         wtime = 100, btime = 100,
                         winc  = 1,   binc  = 1))
    expect_silent(uci_go(engine, infinite = TRUE))

    # Test uci_stop
    expect_silent(uci_stop(engine))

    # Test uci_position
    expect_silent(engine <- uci_position(engine, startpos = TRUE,
                                         moves = 'e2e4 c7c5'))
    expect_silent(engine <- uci_position(engine,
      fen = 'rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1'))

    # Test uci_ponderhit
    expect_silent(engine <- uci_ponderhit(engine))

    # Test uci_read
    expect_silent(engine <- uci_read(engine))
    expect_true(length(engine$temp) > 0)

    # Check the engine
    expect_true(is.list(engine))
    expect_equal(names(engine), c('pipe', 'temp'))
    expect_true(engine$pipe$is_alive())

    # Test uci_quit
    expect_silent(result <- uci_quit(engine))
    expect_type(result, "character")
    expect_error(uci_quit(engine))

  }
})
