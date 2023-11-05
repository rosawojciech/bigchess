test_that("san2lan correctly converts SAN to LAN", {
  expect_equal(san2lan("1. e4 e5 2. Nf3 Nf5 3. d5"), "e2e4 e7e5 g1f3 g8f5 d2d5")
  expect_equal(san2lan("1. e4 c5 2. Nf3 d6 3. d4"), "e2e4 c7c5 g1f3 d7d6 d2d4")
})

test_that("san2lan handles empty input", {
  expect_equal(san2lan(""), "")
})

test_that("san2lan handles non-standard input", {
  expect_error(san2lan("This is not a valid input"))
})
