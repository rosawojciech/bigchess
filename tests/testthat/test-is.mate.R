test_that("is.mate returns correct values", {
  # Set up a specific board position for testing
  position <- position.start()

  # Test 1: No checkmate
  expect_false(is.mate(position, 1)) # White is not in checkmate
  expect_false(is.mate(position, -1)) # Black is not in checkmate

  # Test 2: White is in checkmate
  position[8,8] <- -5 # Place black queen at h1
  position[8,7] <- -5 # Place another black queen at g1
  position[8,6] <-  0 # Remove white bishop at f1
  expect_true(is.mate(position, -1)) # White is in checkmate

  # Test 3: Black is in checkmate
  position <- position.start()
  position[1,1] <- 5 # Place white queen at a8
  position[1,2] <- 0 # Remove black knight b8
  position[1,3] <- 0 # Remove black bishop at c8
  position[1,4] <- 0 # Remove black queen at d8
  expect_true(is.mate(position, 1)) # Black is in checkmate
})
