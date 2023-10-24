test_that("string.lan.move2move converts LAN move correctly", {
  lan_move <- "e2e4"
  expected_move <- c(7, 5, 5, 5, NA)
  actual_move <- string.lan.move2move(lan_move)
  expect_equal(actual_move, expected_move)

  lan_move <- "g7g5"
  expected_move <- c(2, 7, 4, 7, NA)
  actual_move <- string.lan.move2move(lan_move)
  expect_equal(actual_move, expected_move)
})
