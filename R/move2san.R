#' Convert a chess move to Standard Algebraic Notation (SAN)
#'
#' This function takes a chess position and move coordinates, and returns the
#' move in Standard Algebraic Notation (SAN).
#'
#' @details The function evaluates the current state of the game, the positions
#'   of the pieces before and after the move, and any potential promotion. It
#'   then generates a string that uniquely identifies the move in SAN.
#'
#' @param position A matrix representing the current position on the chess
#'   board.
#' @param r1 An integer representing the row of the piece before the move.
#' @param c1 An integer representing the column of the piece before the move.
#' @param r2 An integer representing the row of the piece after the move.
#' @param c2 An integer representing the column of the piece after the move.
#' @param p An optional parameter representing a promotion piece. If a pawn is
#'   promoted as part of the move, `p` should be one of `2` (bishop), `3`
#'   (knight),  `4` (rook), or `5` (queen). If there is no promotion, `p` should
#'   be `NA`.
#'
#' @return A string representing the move in Standard Algebraic Notation (SAN).
#'
#' @export
#'
#' @examples
#' # Initialize a chess board
#' position <- position.start()
#' # Convert a move from (5, 7) to (5, 5) to SAN
#' move2san(position, 5, 7, 5, 5, NA) # Returns "e4"
move2san <- function(position, r1, c1, r2, c2, p) {
  p1 <- position[r1, c1]

  # Check if there is a capture
  if(abs(position[r2, c2]) > 0) {
    capt <- "x"
    if(abs(p1) == 1) { # Capture by a pawn
      capt <- paste0(letters[c1], "x")
    }
  } else {
    capt <- ""
  }

  sc2 <- letters[c2] # File of destination
  sr2 <- c(8:1)[r2] # Row of destination

  m2s <- paste0(casefold(names(figs[abs(p1)]), upper = T),
                disambiguating.move(position, r1, c1, r2, c2), capt, sc2, sr2)

  # Check for castling
  if(abs(p1) == 6 & abs(c1 - c2) > 1){
    if(c2 == 7) { # Short castling
      m2s <- "O-O"
    }
    if(c2 == 3) { # Long castling
      m2s <- "O-O-O"
    }
  }

  # Check for en passant
  if(abs(p1) == 1 & abs(c1 - c2) > 0 & position[r2, c2] == 0) {
    m2s <- paste0(letters[c1], "x", sc2, sr2)
  }

  # Check for promotion
  if(!is.na(p)) {
    m2s <- paste0(capt, sc2, sr2,"=", casefold(names(figs[p]), upper = T))
  }

  return(m2s)
}
