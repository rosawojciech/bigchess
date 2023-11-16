test_that("move2san correctly converts moves", {
  position <- position.start()

  # Test pawn moves
  expect_equal(move2san(position, 5, 7, 5, 5, NA), "e4")
  position[3, 3] <- 3
  expect_equal(move2san(position, 2, 4, 3, 3, NA), "dxc6")
  position[5, 7] <- 1
  position[5, 8] <- -1
  expect_equal(move2san(position, 5, 8, 6, 7, NA), "hxg3")
  position[1, 7] <- 0
  position[2, 7] <- 1
  expect_equal(move2san(position, 2, 7, 1, 7, "b"), "g8=B")

  # Test knight move
  expect_equal(move2san(position, 8, 7, 6, 6, NA), "Nf3")

  # Test bishop move
  position <- position.start()
  position[4,4] <- 2
  expect_equal(move2san(position, 4, 4, 3, 3, NA), "Bc6")

  # Test rook move
  position[5,1] <- 4
  expect_equal(move2san(position, 5, 1, 5, 5, NA), "Re4")

  # Test queen move
  position[4,4] <- 5
  expect_equal(move2san(position, 4, 4, 4, 6, NA), "Qf5")

  # Test king move
  position[5,5] <- 6
  expect_equal(move2san(position, 5, 5, 4, 6, NA), "Kf5")

  # Test castling
  position <- position.start()
  position[8, 2] <- 0
  position[8, 3] <- 0
  position[8, 4] <- 0
  position[8, 6] <- 0
  position[8, 7] <- 0
  expect_equal(move2san(position, 8, 5, 8, 7, NA), "O-O")
  expect_equal(move2san(position, 8, 5, 8, 3, NA), "O-O-O")
})
