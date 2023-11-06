test_that("is.check returns correct values", {
  # Set up a specific board position for testing
  position <- position.start()

  # No check
  expect_false(is.check(position, 1)) # Black is not in check
  expect_false(is.check(position, -1)) # White is not in check
  position <- matrix(
    data = rep(0, times = 64),
    ncol = 8,
    nrow = 8,
    dimnames = list(8:1, letters[1:8])
  )
  position[3, 6] <- -6
  position[2, 5] <- 1
  expect_false(is.check(position, 1)) # Black is not in check

  # White is in check
  position <- position.start()
  position[5,5] <- -5 # Place black queen at e4
  position[7,5] <- 0 # Remove white pawn at e2
  expect_true(is.check(position, -1)) # White is in check

  # Black is in check
  position <- position.start()
  position[4,5] <- 5 # Place white queen at d5
  position[2,5] <- 0 # Remove black pawn at d7
  expect_true(is.check(position, 1)) # Black is in check

  # Checks from diagonals
  # Set up position after e2e4 d7d5 f1b5
  position <- position.start()
  position[7,5] <- 0 # Remove white pawn at e2
  position[5,5] <- 1 # Place white pawn at e4
  position[2,4] <- 0 # Remove black pawn at d7
  position[4,4] <- -1 # Place black pawn at d5
  position[8,6] <- 0 # Remove white bishop at f1
  position[4,2] <- 2 # Place white bishop at b5
  expect_true(is.check(position, 1)) # Black is in check
  position <- matrix(
    data = rep(0, times = 64),
    ncol = 8,
    nrow = 8,
    dimnames = list(8:1, letters[1:8])
  )
  position[5, 5] <- -6
  position[3, 3] <- 2
  expect_true(is.check(position, 1)) # Black is in check
  position[3, 3] <- 0
  position[3, 7] <- 2
  expect_true(is.check(position, 1)) # Black is in check
  position[3, 7] <- 0
  position[7, 7] <- 2
  expect_true(is.check(position, 1)) # Black is in check
  position[7, 7] <- 0
  position[7, 3] <- 5
  expect_true(is.check(position, 1)) # Black is in check

  # Checks from orthogonals
  position[7, 3] <- 0
  position[5, 5] <- 6
  position[5, 3] <- -4
  expect_true(is.check(position, -1)) # White is in check
  position[5, 3] <- 0
  position[3, 5] <- -5
  expect_true(is.check(position, -1)) # White is in check
  position[3, 5] <- 0
  position[5, 7] <- -4
  expect_true(is.check(position, -1)) # White is in check
  position[5, 7] <- 0
  position[7, 5] <- -4
  expect_true(is.check(position, -1)) # White is in check

  # Check from knights
  position[7, 5] <- 0
  position[7, 4] <- -3
  expect_true(is.check(position, -1)) # White is in check
  position[7, 4] <- 0
  position[7, 6] <- -3
  expect_true(is.check(position, -1)) # White is in check
  position[7, 6] <- 0
  position[6, 7] <- -3
  expect_true(is.check(position, -1)) # White is in check
  position[6, 7] <- 0
  position[4, 7] <- -3
  expect_true(is.check(position, -1)) # White is in check
  position[4, 7] <- 0
  position[3, 6] <- -3
  expect_true(is.check(position, -1)) # White is in check
  position[3, 6] <- 0
  position[3, 4] <- -3
  expect_true(is.check(position, -1)) # White is in check
  position[3, 4] <- 0
  position[4, 3] <- -3
  expect_true(is.check(position, -1)) # White is in check
  position[4, 3] <- 0
  position[6, 3] <- -3
  expect_true(is.check(position, -1)) # White is in check

  # Checks from pawns
  position[6, 3] <- 0
  position[4, 4] <- -1
  expect_true(is.check(position, -1)) # White is in check
  position[3, 4] <- 0
  position[4, 6] <- -1
  expect_true(is.check(position, -1)) # White is in check

  # White is in checkmate
  position <- position.start()
  position[8,8] <- -5 # Place black queen at h1
  position[8,7] <- -5 # Place another black queen at g1
  position[8,6] <-  0 # Remove white bishop at f1
  expect_true(is.check(position, -1)) # White is in checkmate
})

