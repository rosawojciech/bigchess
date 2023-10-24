#' Check the validity of a chess move
#'
#' This function evaluates whether a proposed move for a chess piece from its
#' current position to a new position is valid based on the current state of the
#' chess board.
#'
#' @details The function takes as input the current state of the chess board and
#'   the initial and final positions of the piece. It returns a boolean value
#'   indicating whether the proposed move is valid (TRUE) or not (FALSE).
#'
#' @param position A matrix representing the current state of the chess board.
#' @param r1 The rank (row) of the piece's current position.
#' @param c1 The file (column) of the piece's current position.
#' @param r2 The rank (row) of the piece's proposed new position.
#' @param c2 The file (column) of the piece's proposed new position.
#'
#' @return A boolean value: TRUE if the move is valid, FALSE otherwise.
#'
#' @export
#'
#' @examples
#' # Initialize a chess board
#' position <- position.start()
#' # Check if a move from (2, 4) to (4, 4) is possible
#' possible.move(position, r1 = 2, c1 = 4, r2 = 4, c2 = 4)
possible.move <- function(position, r1, c1, r2, c2){
  # Get all possible moves for the piece at [r1, c1]
  pmp <- possible.moves(position, r1, c1)

  # If there are possible moves
  if(nrow(pmp) > 0){
    # Check if [r2, c2] is in the list of possible moves
    return(sum(pmp[, 1] == r2 & pmp[, 2] == c2) > 0)
  } else {
    # If there are no possible moves return FALSE
    return(FALSE)
  }
}
