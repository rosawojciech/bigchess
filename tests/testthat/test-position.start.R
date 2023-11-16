test_that("position.start correctly initializes the starting position", {
  # Initialize the starting position.
  initial_position <- position.start()

  # Test the position of white pawns (should be 1)
  expect_equal(as.vector(initial_position["2", ]), rep(1, 8))

  # Test the position of white rooks (should be 4)
  expect_equal(as.vector(initial_position["1", c("a", "h")]), c(4, 4))

  # Test the position of white knights (should be 3)
  expect_equal(as.vector(initial_position["1", c("b", "g")]), c(3, 3))

  # Test the position of white bishops (should be 2)
  expect_equal(as.vector(initial_position["1", c("c", "f")]), c(2, 2))

  # Test the position of the white queen (should be 5)
  expect_equal(initial_position["1", "d"], 5)

  # Test the position of the white king (should be 6)
  expect_equal(initial_position["1", "e"], 6)

  # Test the position of black pawns (should be -1)
  expect_equal(as.vector(initial_position["7", ]), rep(-1, 8))

  # Test the position of black rooks (should be -4)
  expect_equal(as.vector(initial_position["8", c("a", "h")]), c(-4, -4))

  # Test the position of black knights (should be -3)
  expect_equal(as.vector(initial_position["8", c("b", "g")]), c(-3, -3))

  # Test the position of black bishops (should be -2)
  expect_equal(as.vector(initial_position["8", c("c", "f")]), c(-2, -2))

  # Test the position of the black queen (should be -5)
  expect_equal(initial_position["8", "d"], -5)

  # Test the position of the black king (should be -6)
  expect_equal(initial_position["8", "e"], -6)
})
