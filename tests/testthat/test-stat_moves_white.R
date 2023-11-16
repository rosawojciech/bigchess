test_that("stat_moves_white returns expected output", {
  # Test case with a sequence of white piece moves
  moves <- "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4"
  expected_output <- data.frame(
    W_B_moves = 2,  # Corrected
    W_K_moves = 0,
    W_N_moves = 1,
    W_O_moves = 0,  # Corrected
    W_Q_moves = 0,
    W_R_moves = 0
  )

  actual_output <- stat_moves_white(moves)
  expect_equal(actual_output, expected_output)
})

test_that("stat_moves_white handles no moves", {
  # Test case with no moves
  moves <- ""
  expected_output <- data.frame(
    W_B_moves = 0,
    W_K_moves = 0,
    W_N_moves = 0,
    W_O_moves = 0,
    W_Q_moves = 0,
    W_R_moves = 0
  )

  actual_output <- stat_moves_white(moves)
  expect_equal(actual_output, expected_output)
})
