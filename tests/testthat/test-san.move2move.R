test_that("san.move2move correctly converts SAN moves to coordinates", {

  # Test pawn move
  position <- position.start()
  expect_equal(san.move2move(position, "e4", 1),
               setNames(c(7, 5, 5, 5), c("", "", "4", "e")))
  # Ambiguous pawn capture
  position[3, 5] <- 1
  position[3, 3] <- 1
  position[7, 5] <- 0
  position[7, 3] <- 0
  # Bug? Should be 6 e 7 d?
  expect_equal(san.move2move(position, "ed7", 1),
               setNames(c(3, 5, 2, 4), c("7", "e", "7", "d"))) # e6xd2
  # Promotion with capture
  position <- position.start()
  position[2, 7] <- 1
  position[7, 8] <- 0
  expect_equal(san.move2move(position, "gxf8Q", 1),
               setNames(c(2, 7, NA, NA, 5), c("", "g", NA, NA, "q")))
  position <- position.start()
  position[7, 7] <- -1
  position[2, 8] <- 0
  expect_equal(san.move2move(position, "gxf1Q", -1),
               setNames(c(7, 7, NA, NA, 5), c("", "g", NA, NA, "q")))

  # Test knight move
  position <- position.start()
  expect_equal(san.move2move(position, "Nc3", 1),
               setNames(c(8, 2, 6, 3), c("", "", "3", "c")))
  # Two knights on same column
  position <- position.start()
  position[4, 8] <- 3
  position[6, 8] <- 3
  expect_equal(san.move2move(position, "N5f4", 1),
               setNames(c(4, 8, 5, 6), c("5", "", "4", "f"))) # N5f4

  # Test bishop move
  position[5, 4] <- position[8,3]
  position[8, 3] <- 0
  expect_equal(san.move2move(position, "Bc6", 1),
               setNames(c(8, 6, 3, 3), c("", "", "6", "c")))
  # Two bishops on same diagonal
  position <- position.start()
  position[3, 4] <- 2
  position[5, 6] <- 2
  expect_equal(san.move2move(position, "Bde5", 1),
               setNames(c(3, 4, 4, 5), c("", "d", "5", "e")))

  # Test rook move
  position <- position.start()
  position[4,1] <- position[8,1]
  position[8,1] <- 0
  expect_equal(san.move2move(position, "Ra5", 1),
               setNames(c(8, 8, 4, 1), c("", "", "5", "a")))
  # Two rooks on same row
  position <- position.start()
  position[2, 5] <- 4
  position[2, 3] <- 4
  expect_equal(san.move2move(position, "Red2", 1),
               setNames(c(2, 5, 7, 4), c("", "e", "2", "d"))) # Rexd2
  # Two rooks on same column
  position <- position.start()
  position[4, 8] <- 4
  position[6, 8] <- 4
  expect_equal(san.move2move(position, "R5d2", 1),
               setNames(c(4, 8, 7, 4), c("5", "", "2", "d"))) # R5d2

  # Test queen move
  position <- position.start()
  position[5,4] <- position[8,4]
  position[8,4] <- 0
  expect_equal(san.move2move(position, "Qg4", 1),
               setNames(c(5, 4, 5, 7), c("", "", "4", "g")))
  # Two queens on same row & 2 on same column
  position <- position.start()
  position[3, 3] <- 5 # wQ on c6
  position[5, 3] <- 5 # wQ on c4
  position[3, 5] <- 5 # wQ on e6
  position[5, 5] <- 5 # wQ on e4
  expect_equal(san.move2move(position, "Qc6d5", 1),
               setNames(c(3, 3, 4, 4), c("6", "c", "5", "d"))) # Qc6d5

  # Test castling
  position <- position.start()
  position[8, 2] <- 0
  position[8, 3] <- 0
  position[8, 4] <- 0
  position[8, 6] <- 0
  position[8, 7] <- 0
  expect_equal(san.move2move(position, "O-O",   1), c(8, 5, 8, 7))
  expect_equal(san.move2move(position, "O-O-O", 1), c(8, 5, 8, 3))
  position[1, 2] <- 0
  position[1, 3] <- 0
  position[1, 4] <- 0
  position[1, 6] <- 0
  position[1, 7] <- 0
  expect_equal(san.move2move(position, "O-O",   -1), c(1, 5, 1, 7))
  expect_equal(san.move2move(position, "O-O-O", -1), c(1, 5, 1, 3))

})
