test_that("is.check returns correct values", {
  # Set up a specific board position for testing
  position <- position.start()

  # Test 1: No check
  expect_false(is.check(position, 1)) # White is not in check
  expect_false(is.check(position, -1)) # Black is not in check

  # Test 2: White is in check
  position[5,5] <- -5 # Place black queen at e4
  position[7,5] <- 0 # Remove white pawn at e2
  expect_true(is.check(position, -1)) # White is in check

  # Test 3: Black is in check
  position <- position.start()
  position[4,5] <- 5 # Place white queen at d5
  position[2,5] <- 0 # Remove black pawn at d7
  expect_true(is.check(position, 1)) # Black is in check
})
