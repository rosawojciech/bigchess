test_that("tree_eco correctly computes ECO tree", {
  df <- data.frame(ECO = c("B01", "C00", "B01", "C00", "B01"), Result = c("1-0", "1-0", "0-1", "1/2-1/2", "1-0"))
  expect_equal(tree_eco(df), data.frame(ECO = c("B01", "C00"), White_score = c(66.7, 50), Draws_percent = c(0, 50), Black_score = c(33.3, 0), N = c(3, 2)))
})

test_that("tree_eco handles empty input", {
  df <- data.frame(ECO = character(), Result = character())
  expect_error(tree_eco(df))
})
