convert_factor_columns_to_character <- function(input_df) {
  # Identify the factor columns
  factor_columns <- sapply(input_df, is.factor)

  # Create a copy of the input data frame
  output_df <- input_df

  # Convert only the factor columns to character
  output_df[factor_columns] <- lapply(input_df[factor_columns], as.character)

  return(output_df)
}

test_that("extract_moves works", {
  movetext <- "1. e4 e5 2. Nf3 Nf5 3. d5 "
  expected_output <- data.frame(
    W1 = "e4",
    B1 = "e5",
    W2 = "Nf3",
    B2 = "Nf5",
    W3 = "d5",
    B3 = NA_character_,
    complete.movetext = TRUE
  )
  output <- extract_moves(movetext, N = 3, last.move = FALSE)
  output <- convert_factor_columns_to_character(output)
  expect_equal(output, expected_output)
})

test_that("extract_moves handles last.move parameter correctly", {
  movetext <- "1. e4 e5 2. Nf3 Nf5 3. d5 "
  expected_output_last_move_true <- data.frame(
    W1 = "e4",
    B1 = "e5",
    W2 = "Nf3",
    B2 = "Nf5",
    W3 = "d5",
    B3 = NA_character_,
    last.move = "d5",
    complete.movetext = TRUE
  )
  expected_output_last_move_false <- data.frame(
    W1 = "e4",
    B1 = "e5",
    W2 = "Nf3",
    B2 = "Nf5",
    W3 = "d5",
    B3 = NA_character_,
    complete.movetext = TRUE
  )
  output_last_move_true  <- extract_moves(movetext, N = 3, last.move = TRUE)
  output_last_move_false <- extract_moves(movetext, N = 3, last.move = FALSE)

  output_last_move_true <- convert_factor_columns_to_character(output_last_move_true)
  output_last_move_false <- convert_factor_columns_to_character(output_last_move_false)

  expect_equal(output_last_move_true,  expected_output_last_move_true)
  expect_equal(output_last_move_false, expected_output_last_move_false)
})
