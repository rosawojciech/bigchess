test_that("disambiguating.move returns correct values", {

  # Unambiguous move
  position <- position.start()
  expect_equal(disambiguating.move(position, 5, 2, 5, 4), "") # e4

  # Two bishops on same diagonal
  position[3, 4] <- 2
  position[5, 6] <- 2
  expect_equal(disambiguating.move(position, 3, 4, 4, 5), "d") # Bde5

  # Two rooks on same row
  position <- position.start()
  position[2, 5] <- 4
  position[2, 3] <- 4
  expect_equal(disambiguating.move(position, 2, 5, 2, 4), "e") # Rexd2

  # Two rooks on same column
  position <- position.start()
  position[4, 8] <- 4
  position[6, 8] <- 4
  expect_equal(disambiguating.move(position, 4, 8, 5, 8), "5") # R5d2

  # Two queens on same row & 2 on same column
  position <- position.start()
  position[3, 3] <- 5 # wQ on c6
  position[5, 3] <- 5 # wQ on c4
  position[3, 5] <- 5 # wQ on e6
  position[5, 5] <- 5 # wQ on e4
  expect_equal(disambiguating.move(position, 3, 3, 4, 4), "c6") # Qc6d5

  # Two knights on same column
  position <- position.start()
  position[4, 8] <- 3
  position[6, 8] <- 3
  expect_equal(disambiguating.move(position, 4, 8, 5, 6), "5") # N5f4

  # Pawn moves need no disambiguation (two pawns on same row)
  position <- position.start()
  position[3, 5] <- 1
  position[3, 3] <- 1
  expect_equal(disambiguating.move(position, 3, 5, 2, 4), "") # exd2
})
