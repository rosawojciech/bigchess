#' Convert a chess move to Long Algebraic Notation (LAN)
#'
#' This function converts a chess move from coordinate format to Long Algebraic
#' Notation (LAN).
#'
#' @details The function takes the row and column of the piece before and after
#'   the move, as well as an optional parameter for pawn promotion. If a pawn is
#'   promoted during the move, the promotion piece should be specified as `2`
#'   (bishop), `3` (knight), `4` (rook), or `5` (queen). If there is no
#'   promotion, set `pr` to `NA`.
#'
#' @param r1 An integer representing the row of the piece before the move.
#' @param c1 An integer representing the column of the piece before the move.
#' @param r2 An integer representing the row of the piece after the move.
#' @param c2 An integer representing the column of the piece after the move.
#' @param pr An optional parameter representing a promotion piece.
#'
#' @return A string representing the move in Long Algebraic Notation (LAN).
#'
#' @export
#'
#' @examples
#' move2lan(2, 5, 4, 5, NA) # Returns "e2e4"
move2lan <- function(r1, c1, r2, c2, pr) {
  # Convert the row and column coordinates to Long Algebraic Notation (LAN)
  lan <- paste0(letters[c1], 9 - r1, letters[c2], 9 - r2)

  # If there is a pawn promotion, append the promotion piece to the LAN
  if (!is.na(pr)) {
    lan <- paste0(lan, casefold(names(figs)[abs(pr)], upper = TRUE))
  }

  return(lan)
}
