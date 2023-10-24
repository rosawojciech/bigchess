test_that("uci_parse correctly parses GUI commands from chess engine", {
  ucilog <- c("info depth 1 seldepth 1 multipv 1 score cp 20 nodes 20 nps 20000 tbhits 0 time 1 pv e2e4",
              "info depth 2 seldepth 2 multipv 1 score cp -10 nodes 400 nps 200000 tbhits 0 time 2 pv e7e5 g1f3",
              "bestmove e2e4 ponder e7e5")

  expect_equal(uci_parse(ucilog, filter = "bestmove"), "e2e4")
  expect_equal(uci_parse(ucilog, filter = "score"), -10)
  expect_equal(uci_parse(ucilog, filter = "bestline"), "e7e5 g1f3")
})

test_that("uci_parse handles empty ucilog", {
  ucilog <- character()

  expect_equal(uci_parse(ucilog, filter = "bestmove"), character(0))
  expect_equal(uci_parse(ucilog, filter = "score"), integer(0))
  expect_equal(uci_parse(ucilog, filter = "bestline"), character(0))
})

test_that("uci_parse handles non-standard ucilog", {
  ucilog <- c("This is not a valid ucilog")

  expect_equal(uci_parse(ucilog, filter = "bestmove"), character(0))
})
