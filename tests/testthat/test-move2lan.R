test_that("move2lan correctly converts moves to long algebraic notation", {
  # Test pawn move
  expect_equal(move2lan(2, 5, 4, 5, NA), "e7e5")

  # Test knight move
  expect_equal(move2lan(1, 2, 3, 3, NA), "b8c6")

  # Test bishop move
  expect_equal(move2lan(1, 3, 4, 4, NA), "c8d5")

  # Test rook move
  expect_equal(move2lan(1, 1, 5, 1, NA), "a8a4")

  # Test queen move
  expect_equal(move2lan(1, 4, 4, 6, NA), "d8f5")

  # Test king move
  expect_equal(move2lan(1, 5, 2, 5, NA), "e8e7")
})
test_that("move2lan correctly converts pawn promotion to long algebraic notation", {
  # Test pawn promotion to white queen
  expect_equal(move2lan(7, 5, 8, 5, 5), "e2e1Q")

  # Test pawn promotion to white rook
  expect_equal(move2lan(7, 5, 8, 5, 4), "e2e1R")

  # Test pawn promotion to bishop for black
  expect_equal(move2lan(2, 5, 1, 5, -2), "e7e8B")

  # Test pawn promotion to knight for black
  expect_equal(move2lan(2, 5, 1, 5, -3), "e7e8N")
})
