#' Check if a king is in check
#'
#' This function checks if a king is in check. It does this by checking if any
#' of the opponent's pieces can move to the square currently occupied by the
#' king.
#'
#' @details The function takes into account the current state of the game and
#'   the color of the king in question. It then checks all possible moves of the
#'   opponent's pieces to see if any could capture the king.
#'
#' @param position A matrix representing the current state of the chess board.
#' @param p An integer indicating the color of the king to check (1 for white,
#'   -1 for black).
#'
#' @return A boolean value indicating whether the king is in check (TRUE) or not
#'   (FALSE).
#'
#' @export
#'
#' @examples
#' # Initialize a chess board
#' position <- position.start()
#' # Check if the white king is in check
#' is.check(position, 1)
is.check <- function(position, p) {
  # Locate the king
  king <- arrayInd(match(-p*6, position), .dim = dim(position))

  # Check for attacking enemy pawns
  pawn_positions <- matrix(c(-1, -1, 1, -1), ncol = 2) * -p
  for (i in 1:nrow(pawn_positions)) {
    x <- king[1] + pawn_positions[i, 1]
    y <- king[2] + pawn_positions[i, 2]
    if (x >= 1 && x <= 8 && y >= 1 && y <= 8 && position[x, y] == p) {
      return(TRUE)
    }
  }

  # Check for attacking enemy knights
  knight_positions <- matrix(c(-2, -1, -2, 1, 2, -1, 2, 1, -1, -2, -1, 2, 1, -2, 1, 2), ncol = 2, byrow = TRUE)
  for (i in 1:nrow(knight_positions)) {
    x <- king[1] + knight_positions[i, 1]
    y <- king[2] + knight_positions[i, 2]
    if (x >= 1 && x <= 8 && y >= 1 && y <= 8 && position[x, y] == p*3) {
      return(TRUE)
    }
  }

  # Check each orthogonal direction
  rook_directions <- matrix(c(-1, 0, 1, 0, 0, -1, 0, 1), ncol = 2)
  for (i in 1:nrow(rook_directions)) {
    x <- king[1] + rook_directions[i, 1]
    y <- king[2] + rook_directions[i, 2]
    while (x >= 1 && x <= 8 && y >= 1 && y <= 8 && position[x, y] == 0) {
      x <- x + rook_directions[i, 1]
      y <- y + rook_directions[i, 2]
    }
    if (x >= 1 && x <= 8 && y >= 1 && y <= 8 && (position[x, y] == p*4 || position[x, y] == p*5)) {
      return(TRUE)
    }
  }

  # Check each diagonal direction
  bishop_directions <- matrix(c(-1, -1, -1, 1, 1, -1, 1, 1), ncol = 2, byrow = TRUE)
  for (i in 1:nrow(bishop_directions)) {
    x <- king[1] + bishop_directions[i, 1]
    y <- king[2] + bishop_directions[i, 2]
    while (x >= 1 && x <= 8 && y >= 1 && y <= 8 && position[x, y] == 0) {
      x <- x + bishop_directions[i, 1]
      y <- y + bishop_directions[i, 2]
    }
    if (x >= 1 && x <= 8 && y >= 1 && y <= 8 && (position[x, y] == p*2 || position[x, y] == p*5)) {
      return(TRUE)
    }
  }

  # If no attacks are found, return false
  return(FALSE)
}
