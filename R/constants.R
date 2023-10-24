# Define row numbers, column letters, and piece symbols
rn <- 1:8
names(rn) <- 8:1
cn <- 1:8
names(cn) <- letters[1:8]
figs <- 1:6
names(figs) <- c("", "b", "n", "r", "q", "k")

# Define knight moves
kni <- matrix(data = c(-1, -1, -2, -2, 1, 1, 2, 2,
                       2, -2, 1, -1, 2, -2, 1, -1),
              nrow = 8, ncol = 2)
# Define king moves
kin <- matrix(data = c(-1, -1, -1, 0, 0, 1, 1, 1,
                       -1, 0, 1, -1, 1, -1, 0, 1),
              nrow = 8, ncol = 2)

# Define the start position
ps <- matrix(
  data = rep(0, times = 64),
  ncol = 8,
  nrow = 8,
  dimnames = list(8:1, letters[1:8])
)
# Place white pawns
ps["2", ] <- 1
# Place white pieces: rook (4), knight (3), bishop (2), queen (5), king (6)
ps["1", ] <- c(4, 3, 2, 5, 6, 2, 3, 4)
# Place black pawns and pieces with negative sign to indicate black color
ps["7", ] <- -1
ps["8", ] <- -c(4, 3, 2, 5, 6, 2, 3, 4)

# Define diagonal moves
diag1 <- row(ps) - col(ps)
diag2 <- -diag1[, 8:1]
