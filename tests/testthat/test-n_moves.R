test_that("n_moves correctly computes number of moves", {
  # Test with a single movetext string
  expect_equal(n_moves("1. e4 e5 2. Nf3 Nf5 3. d5 "), 3)

  # Test with a vector of movetext strings
  expect_equal(n_moves(c("1. e4 e5 2. Nf3 Nf5 3. d5 ", "1. d4 d5")), c(3, 1))

  # Test with a movetext string that has one move
  expect_equal(n_moves("1. e4"), 1)

  # Test with a movetext string that has multiple moves with the same number
  expect_equal(n_moves("1. e4 e5 1. Nf3 Nf5"), 1)

  # Test with an empty movetext string
  expect_equal(n_moves(""), 0)
})
