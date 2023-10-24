test_that("possible.moves returns correct values", {
  # Set up a specific board position for testing
  position <- position.start()

  # Test 1: Possible moves for a pawn at the start of the game
  pawn_moves <- possible.moves(position, 2, 1) # White pawn at a2
  expect_equal(pawn_moves, matrix(c(3, 1, 4, 1), ncol = 2, byrow = TRUE)) # Can move to a3 or a4

  # Test 2: Possible moves for a knight at the start of the game
  knight_moves <- possible.moves(position, 1, 2) # White knight at b1
  expect_equal(knight_moves, matrix(c(3, 3, 3, 1), ncol = 2, byrow = TRUE)) # Can move to c3 or a3

  # Test 3: No possible moves for a queen at the start of the game
  queen_moves <- possible.moves(position, 1, 5) # White queen at e1
  expect_equal(queen_moves, matrix(ncol = 2)[-1,]) # No legal moves
})
