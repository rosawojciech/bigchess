#' Compute possible moves for a piece
#'
#' This function calculates all possible moves for a specific piece on the chess
#' board. It takes into account the type of the piece and the current state of
#' the board.
#'
#' @details The function evaluates the current state of the game and the
#'   position of the piece in question. It then generates all potential moves
#'   for this piece, considering the rules of movement for that particular
#'   piece.
#'
#' @param position A matrix representing the current state of the chess board.
#' @param r1,c1 The rank and file of the piece's current position.
#'
#' @return A matrix where each row represents a possible move. Each row contains
#'   two elements: the rank and file of the destination square.
#'
#' @export
#'
#' @examples
#' # Initialize a chess board
#' position <- position.start()
#' # Compute possible moves for a piece at (2, 4)
#' possible.moves(position, r1 = 2, c1 = 4)
possible.moves <- function(position, r1, c1) {
  # Initialize an empty matrix to store possible moves
  psmv <- matrix(ncol = 2)[-1, ]

  # Get the value of the piece at the given position
  p1 <- position[r1, c1]

  # If it's not an empty square or a king
  if(abs(p1) < 7) {
    # Compute all checking moves for this piece
    rslt <- checking.moves(position, r1, c1)

    # If there are any checking moves
    if(nrow(rslt) > 0) {
      # Loop through all checking moves
      for(ff in 1:nrow(rslt)) {
        # Simulate each move
        tmpp <- position.move(position, r1, c1, rslt[ff, 1], rslt[ff, 2])

        # If this move doesn't put our own king in check
        if(!is.check(tmpp, sign(-p1))) {
          # Add this move to possible moves
          psmv <- rbind(psmv, rslt[ff, ])
        }
      }
    }
  }

  return(psmv)
}
