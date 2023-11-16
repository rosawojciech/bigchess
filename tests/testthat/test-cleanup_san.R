test_that("cleanup_san removes move numbers", {
  expect_equal(cleanup_san("1.e4"), "e4")
  expect_equal(cleanup_san("10. e4 Qd8"), "e4 Qd8")
})

test_that("cleanup_san removes check and checkmate symbols", {
  expect_equal(cleanup_san("e4+"), "e4")
  expect_equal(cleanup_san("Qxh7#"), "Qh7")
})

test_that("cleanup_san removes capture symbol", {
  expect_equal(cleanup_san("exd5"), "ed5")
})

test_that("cleanup_san removes extra spaces", {
  expect_equal(cleanup_san(" e4 "), "e4")
  expect_equal(cleanup_san("  e4 e5 "), "e4 e5")
})

test_that("cleanup_san does not remove piece letters or square coordinates", {
  expect_equal(cleanup_san("Nf3"), "Nf3")
  expect_equal(cleanup_san("e5"), "e5")
})
