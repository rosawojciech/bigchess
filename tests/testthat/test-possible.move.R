# Define your tests
test_that("possible.move returns correct values", {
  # Set up a specific board position for testing
  position <- position.start()

  # Test 1: A pawn's typical move
  expect_true(possible.move(position, 2, 1, 3, 1)) # White pawn at a2 to a3

  # Test 2: A pawn's first double move
  expect_true(possible.move(position, 2, 1, 4, 1)) # White pawn at a2 to a4

  # Test 3: A pawn's capturing move
  position[5,5] <- -1 # Place a black pawn at e4
  position[6,4] <- 1 # Place a white pawn at d5
  expect_true(possible.move(position, 6, 4, 5, 5)) # White pawn at d5 captures on e4

  # Test 4: A move that is not possible
  expect_false(possible.move(position, 2, 1, 4, 2)) # White pawn at a2 to b4 is not possible
})
