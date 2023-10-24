test_that("checking.moves works for rook", {
  # Create a chessboard with a rook at position (1,1)
  position <- matrix(0, nrow = 8, ncol = 8)
  position[1, 1] <- 4

  # Define the expected output
  expected_output <- rbind(cbind(rep(1, 7), 2:8), cbind(2:8, rep(1, 7)))

  # Call the checking.moves function
  actual_output <- checking.moves(position, 1, 1)

  # Check if the actual output matches the expected output
  expect_equal(actual_output, expected_output)
})

test_that("checking.moves works for bishop", {
  # Create a chessboard with a bishop at position (1,1)
  position <- matrix(0, nrow = 8, ncol = 8)
  position[1, 1] <- 2

  # Define the expected output
  expected_output <- cbind(2:8, 2:8)

  # Call the checking.moves function
  actual_output <- checking.moves(position, 1, 1)

  # Check if the actual output matches the expected output
  expect_equal(actual_output, expected_output)
})

test_that("checking.moves works for knight", {
  # Create a chessboard with a knight at position (1,1)
  position <- matrix(0, nrow = 8, ncol = 8)
  position[1, 1] <- 3

  # Define the expected output
  expected_output <- rbind(c(3,2), c(2,3))

  # Call the checking.moves function
  actual_output <- checking.moves(position, 1, 1)

  # Sort both matrices by their first column
  expected_output <- expected_output[order(expected_output[,1]),]
  actual_output <- actual_output[order(actual_output[,1]),]

  # Check if the actual output matches the expected output
  expect_equal(actual_output, expected_output)
})


test_that("checking.moves works for queen", {
  # Create a chessboard with a queen at position (1,1)
  position <- matrix(0, nrow = 8, ncol = 8)
  position[1, 1] <- 5

  # Define the expected output
  expected_output <- rbind(cbind(rep(1,7),2:8),cbind(2:8,rep(1,7)),cbind(2:8,2:8))

  # Call the checking.moves function
  actual_output <- checking.moves(position, 1, 1)

  # Check if the actual output matches the expected output
  expect_equal(actual_output, expected_output)
})

test_that("checking.moves works for king", {
  # Create a chessboard with a king at position (1,1)
  position <- matrix(0, nrow = 8, ncol = 8)
  position[1, 1] <- 6

  # Define the expected output
  expected_output <- rbind(c(1,2), c(2,1), c(2,2))

  # Call the checking.moves function
  actual_output <- checking.moves(position, 1, 1)

  # Sort both matrices by their first column
  expected_output <- expected_output[order(expected_output[,1]),]
  actual_output <- actual_output[order(actual_output[,1]),]

  # Check if the actual output matches the expected output
  expect_equal(actual_output, expected_output)
})

test_that("checking.moves works for pawn", {
  # Create a chessboard with a pawn at position (2,5)
  position <- matrix(0, nrow = 8, ncol = 8)
  position[7, 5] <- 1
  position[6, 4] <- -1

  # Define the expected output
  expected_output <- rbind(c(6,5), c(5,5), c(6,4))

  # Call the checking.moves function
  actual_output <- checking.moves(position, 7,5)

  # Sort both matrices by their first column
  expected_output <- expected_output[order(expected_output[,1]),]
  actual_output <- actual_output[order(actual_output[,1]),]

  # Check if the actual output matches the expected output
  expect_equal(actual_output, expected_output)
})
