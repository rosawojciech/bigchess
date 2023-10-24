test_that("stat_moves correctly extracts move statistics", {
  expect_equal(stat_moves("1. e4 e5 2. Nf3 Nf5 3. d5 ", "both"),
               data.frame(W_B_moves = 0, W_K_moves = 0, W_N_moves = 1,
                          W_O_moves = 0, W_Q_moves = 0, W_R_moves = 0,
                          B_B_moves = 0, B_K_moves = 0, B_N_moves = 1,
                          B_O_moves = 0, B_Q_moves = 0, B_R_moves = 0,
                            B_moves = 0,   K_moves = 0,   N_moves = 2,
                            O_moves = 0,   Q_moves = 0,   R_moves = 0))
  expect_equal(stat_moves("1. e4 e5 2. Nf3 Nf5 3. d5 ", "black"),
               data.frame(B_B_moves = 0, B_K_moves = 0, B_N_moves = 1,
                          B_O_moves = 0, B_Q_moves = 0, B_R_moves = 0))
  expect_equal(stat_moves("1. e4 c5 2. Nf3 d6 3. d4", "white"),
               data.frame(W_B_moves = 0, W_K_moves = 0, W_N_moves = 1,
                          W_O_moves = 0, W_Q_moves = 0, W_R_moves = 0))
})

test_that("stat_moves handles empty input", {
  expect_equal(stat_moves("", "both"),
               data.frame(W_B_moves = 0, W_K_moves = 0, W_N_moves = 0,
                          W_O_moves = 0, W_Q_moves = 0, W_R_moves = 0,
                          B_B_moves = 0, B_K_moves = 0, B_N_moves = 0,
                          B_O_moves = 0, B_Q_moves = 0, B_R_moves = 0,
                            B_moves = 0,   K_moves = 0,   N_moves = 0,
                            O_moves = 0,   Q_moves = 0,   R_moves = 0))
})
