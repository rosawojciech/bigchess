test_that("position.move correctly moves a chess piece", {
  # Test case with moving a white pawn from (2, 2) to (4, 2)
  board <- matrix(0, nrow = 8, ncol = 8)
  board[2, 2] <- 1  # White pawn at (2, 2)
  expected_board <- matrix(0, nrow = 8, ncol = 8)
  expected_board[4, 2] <- 1  # White pawn at (4, 2)

  actual_board <- position.move(board, 2, 2, 4, 2)
  expect_equal(actual_board, expected_board)
})

test_that("position.move correctly handles short castling", {
  # Test case with white short castling (king's side)
  board <- matrix(0, nrow = 8, ncol = 8)
  board[1, 5] <- 6  # White king at (1, 5)
  board[1, 8] <- 4  # White rook at (1, 8)
  expected_board <- matrix(0, nrow = 8, ncol = 8)
  expected_board[1, 7] <- 6  # White king at (1, 7)
  expected_board[1, 8] <- 0  # White rook at (1, 8)
  expected_board[1, 6] <- 4  # White rook at (1, 6)

  actual_board <- position.move(board, 1, 5, 1, 7)
  expect_equal(actual_board, expected_board)
})

test_that("position.move correctly handles en passant", {
  # Test case with white pawn capturing en passant
  board <- matrix(0, nrow = 8, ncol = 8)
  board[5, 4] <- -1  # Black pawn at (4, 4)
  board[5, 5] <- 1   # White pawn at (5, 5)
  expected_board <- matrix(0, nrow = 8, ncol = 8)
  expected_board[4, 4] <- 0  # Black pawn at (4, 4)
  expected_board[5, 5] <- 0  # White pawn at (5, 5)
  expected_board[6, 4] <- 1  # White pawn at (4, 5)

  actual_board <- position.move(board, 5, 5, 6, 4)
  expect_equal(actual_board, expected_board)
})

test_that("position.move correctly handles promotion", {
  # Test case with promoting a white pawn to a queen
  board <- matrix(0, nrow = 8, ncol = 8)
  board[7, 4] <- 1  # White pawn at (7, 4)
  expected_board <- matrix(0, nrow = 8, ncol = 8)
  expected_board[8, 4] <- 5  # White queen at (8, 4)

  actual_board <- position.move(board, 7, 4, 8, 4, p = 5)
  expect_equal(actual_board, expected_board)
})

test_that("position.move correctly handles long castling", {
  # Test case for long castling with white
  board <- matrix(0, nrow = 8, ncol = 8)
  board[1, 5] <- 6   # White king at (1, 5)
  board[1, 1] <- 4   # White rook at (1, 1)
  expected_board <- matrix(0, nrow = 8, ncol = 8)
  expected_board[1, 3] <- 6  # White king at (1, 3)
  expected_board[1, 4] <- 4  # White rook at (1, 4)
  expected_board[1, 1] <- 0  # White rook at (1, 1)

  actual_board <- position.move(board, 1, 5, 1, 3)
  expect_equal(actual_board, expected_board)
})
