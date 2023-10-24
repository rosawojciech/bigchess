#' Convert a chess move from standard algebraic notation (SAN) to coordinates
#'
#' This function converts a chess move from SAN to a more computational-friendly
#' coordinate format.
#'
#' @details The function takes a chess position and a move in SAN as input. The
#'   row numbers in the output are inverted compared to the traditional chess
#'   board, with 1 at the top and 8 at the bottom. The output is a named integer
#'   vector of length 4 or 5, depending on whether a pawn promotion occurs
#'   during the move.
#'
#' @param position A matrix representing the current position on the chess
#'   board.
#' @param san.move A string representing the move in standard algebraic notation
#'   (SAN).
#' @param p An integer representing the player (1 for white, -1 for black).
#'
#' @return A named integer vector representing the move in coordinate format.
#'   The vector contains four or five elements: the row and column of the piece
#'   before the move, the row and column of the piece after the move, and an
#'   optional promotion piece. If a pawn is promoted as part of the move, the
#'   fifth element should be one of `2` (bishop), `3` (knight), `4` (rook), or
#'   `5` (queen). If there is no promotion, the vector will have length 4. The
#'   names of the elements represent their coordinates in SAN.
#'
#' @export
#'
#' @examples
#' position <- position.start()
#' san.move2move(position, "e4", 1) # Returns c(7, 5, 5, 5) with names c("", "", "4", "e")
san.move2move <- function(position, san.move, p) {
  # Define the patterns to match different types of chess moves
  ptrns <- c("[a-z]+[0-9]", "[A-Z][a-z]+[0-9]", "[A-Z][0-9][a-z][0-9]",
             "[A-Z][a-z][0-9][a-z][0-9]", "O-O", "O-O-O", "[a-z]+[0-9]=[A-Z]")

  # Iterate through the defined patterns to find a match for the SAN move
  for (fp in 1:length(ptrns)) {
    # Use regular expressions to match the current pattern
    m <- gregexpr(paste0("^", ptrns[fp], "$"), san.move)

    # If there is a match, exit the loop
    if (length(regmatches(san.move, m)[[1]]) > 0) break()
  }
  sm <- strsplit(san.move, "")[[1]]
  lsm <- length(sm)
  pr <- NULL

  # Determine the move type and calculate the corresponding chess move based on
  # the matched pattern (fp)
  switch(fp,
         '1' = { # Pawn move
           if (lsm == 2) {
             r2 <- rn[sm[2]]
             c2 <- cn[sm[1]]
             wp <- which(position == p, arr.ind = TRUE)
             wpp <- matrix(wp[wp[, 2] == c2, ], ncol = 2)

             for (fr in 1:nrow(wpp)) {
               if (possible.move(position, wpp[fr, 1], wpp[fr, 2], r2, c2)) break()
             }
             r1 <- wpp[fr, 1]
             c1 <- wpp[fr, 2]
           }
           if (lsm == 3) { # Pawn move with capture
             r2 <- rn[sm[3]]
             c2 <- cn[sm[2]]
             c1 <- cn[sm[1]]
             r1 <- r2 + p
           }
         },
         '2' = { # Figure move
           if (lsm == 3) { # Figure move without disambiguating moves
             r2 <- rn[sm[3]]
             c2 <- cn[sm[2]]
             fig <- figs[casefold(sm[1])] * p
             wp <- which(position == fig, arr.ind = TRUE)

             for (fr in 1:nrow(wp)) {
               if (possible.move(position, wp[fr, 1], wp[fr, 2], r2, c2)) break()
             }
             r1 <- wp[fr, 1]
             c1 <- wp[fr, 2]
           }
           if (lsm == 4) { # Figure move with file information
             r2 <- rn[sm[4]]
             c2 <- cn[sm[3]]
             c1 <- cn[sm[2]]
             fig <- figs[casefold(sm[1])] * p
             wp <- which(position == fig, arr.ind = TRUE)
             wpp <- matrix(wp[wp[, 2] == c1, ], ncol = 2)

             for (fr in 1:nrow(wpp)) {
               if (possible.move(position, wpp[fr, 1], c1, r2, c2)) break()
             }
             r1 <- wpp[fr, 1]
           }
         },
         '3' = {
           if (lsm == 4) { # Figure move with rank information
             r2 <- rn[sm[4]]
             c2 <- cn[sm[3]]
             r1 <- rn[sm[2]]
             fig <- figs[casefold(sm[1])] * p
             wp <- which(position == fig, arr.ind = TRUE)
             wpp <- matrix(wp[wp[, 1] == r1, ], ncol = 2)

             for (fr in 1:nrow(wpp)) {
               if (possible.move(position, r1, wpp[fr, 2], r2, c2)) break()
             }
             c1 <- wpp[fr, 2]
           }
         },
         '4' = { # Rare case: Figure move with file and rank information
           r2 <- rn[sm[5]]
           c2 <- cn[sm[4]]
           r1 <- rn[sm[3]]
           c1 <- cn[sm[2]]
         },
         '5' = { # Short castling
           c2 <- 7
           if (p == 1) r1 <- r2 <- 8
           if (p == -1) r1 <- r2 <- 1
           c1 <- 5
         },
         '6' = { # Long castling
           c2 <- 3
           if (p == 1) r1 <- r2 <- 8
           if (p == -1) r1 <- r2 <- 1
           c1 <- 5
         },
         '7' = { # Promotion
           if (lsm == 4) { # Promotion without capture
             pr <- figs[casefold(sm[4])]
             r2 <- rn[sm[2]]
             c1 <- c2 <- cn[sm[1]]
           }
           if (lsm == 5) { # Promotion with capture
             pr <- figs[casefold(sm[5])]
             r2 <- rn[sm[3]]
             c2 <- cn[sm[2]]
             c1 <- cn[sm[1]]
           }
           if (p == 1) r1 <- 2
           if (p == -1) r1 <- 7
         }
  )
  return(c(r1, c1, r2, c2, pr))
}

