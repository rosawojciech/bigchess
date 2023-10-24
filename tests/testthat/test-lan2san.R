test_that("lan2san works", {

  # Check
  san_result <- lan2san("e2e4 d7d5 f1b5")
  expect_equal(san_result, "1. e4 d5 2. Bb5+")

  # Mate
  san_result <- lan2san("g2g4 e7e5 f2f4 d8h4")
  expect_equal(san_result, "1. g4 e5 2. f4 Qh4#")

})
