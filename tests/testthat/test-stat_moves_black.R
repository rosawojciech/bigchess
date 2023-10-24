test_that("stat_moves_black returns expected output", {
  # Test case with a sequence of black piece moves
  moves <- "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4"
  expected_output <- data.frame(
    B_B_moves = 0,
    B_K_moves = 0,
    B_N_moves = 1,
    B_O_moves = 0,
    B_Q_moves = 0,
    B_R_moves = 0
  )

  actual_output <- stat_moves_black(moves)
  expect_equal(actual_output, expected_output)
})

test_that("stat_moves_black handles no moves", {
  # Test case with no moves
  moves <- ""
  expected_output <- data.frame(
    B_B_moves = 0,
    B_K_moves = 0,
    B_N_moves = 0,
    B_O_moves = 0,
    B_Q_moves = 0,
    B_R_moves = 0
  )

  actual_output <- stat_moves_black(moves)
  expect_equal(actual_output, expected_output)
})
