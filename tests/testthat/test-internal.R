# Test 1: Check when `xp` is less than the length of `vec` and there are other pieces to the right
test_that("xp less than length of vec, other pieces to the right", {
  vec <- c(0, 1, 0, -1, 0)
  xp <- 2
  expected_output <- c(3, 4, 1)
  expect_equal(internal(vec, xp), expected_output)
})

# Test 2: Check when `xp` is less than the length of `vec` and there are no other pieces to the right
test_that("xp less than length of vec, no other pieces to the right", {
  vec <- c(0, 1, 0, 0, 0)
  xp <- 2
  expected_output <- c(3, 4, 5, 1)
  expect_equal(internal(vec, xp), expected_output)
})

# Test 3: Check when `xp` is greater than 1 and there are other pieces to the left
test_that("xp greater than 1, other pieces to the left", {
  vec <- c(0, -1, 0, 1, 0)
  xp <- 4
  expected_output <- c(5, 2, 3)
  expect_equal(internal(vec, xp), expected_output)
})

# Test 4: Check when `xp` is greater than 1 and there are no other pieces to the left
test_that("xp greater than 1, no other pieces to the left", {
  vec <- c(0,0,-1,-1,-1)
  xp <- length(vec)
  expected_output <- NULL
  expect_equal(internal(vec,xp),expected_output)
})

# Test5: Check when `xp` is equal to the length of `vec`
test_that("xp equal to length of vec", {
  vec <- c(0,-1,-1,-1)
  xp <- length(vec)
  expected_output <- NULL
  expect_equal(internal(vec,xp),expected_output)
})

# Test6: Check when `xp` is equal to one
test_that("xp equal to one", {
  vec <- c(-1,-1,-1,-1)
  xp <- length(vec)
  expected_output <- NULL
  expect_equal(internal(vec,xp),expected_output)
})

