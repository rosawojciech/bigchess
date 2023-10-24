test_that("print.pos correctly prints the initial chess position", {
  initial_position <- matrix(c(
    -4, -3, -2, -5, -6, -2, -3, -4,
    -1, -1, -1, -1, -1, -1, -1, -1,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 1, 1, 1, 1, 1,
    4, 3, 2, 5, 6, 2, 3, 4
  ), nrow = 8, byrow = TRUE)

  # Redirect the console output for comparison
  sink(file = "test_output.txt")
  print.pos(initial_position)
  sink()

  # Read the printed output from the file
  actual_output <- readLines("test_output.txt")
  file.remove("test_output.txt")

  # Define the expected output
  expected_output <- c(
    "♜♞♝♛♚♝♞♜",
    "♟♟♟♟♟♟♟♟",
    "⛝⛝⛝⛝⛝⛝⛝⛝",
    "⛝⛝⛝⛝⛝⛝⛝⛝",
    "⛝⛝⛝⛝⛝⛝⛝⛝",
    "⛝⛝⛝⛝⛝⛝⛝⛝",
    "♙♙♙♙♙♙♙♙",
    "♖♘♗♕♔♗♘♖"
  )

  # Compare the actual and expected output
  expect_equal(actual_output, expected_output)
})
