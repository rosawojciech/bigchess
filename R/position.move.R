#' Move a chess piece
#'
#' This function moves a chess piece from its current position to a specified
#' destination on the chess board.
#'
#' @details The function first identifies the pieces at the source and
#'   destination positions. It then moves the piece from the source to the
#'   destination.  It also handles special moves such as castling, en passant,
#'   and promotion. If the move is a castling move (identified by a king moving
#'   more than one square), it adjusts the rook's position based on whether it's
#'   short or long castling. If the move is an en passant (identified by a pawn
#'   moving diagonally to an empty square), it removes the opponent's pawn. If a
#'   promotion is specified, it changes the pawn at the destination to the
#'   specified piece.
#'
#' @param position A matrix representing the current state of the chess board.
#' @param r1,c1 The rank and file of the piece's current position.
#' @param r2,c2 The rank and file of the piece's destination.
#' @param p (optional) The piece to be promoted to, represented as a number. If
#'   not provided, no promotion occurs.
#'
#' @return A matrix representing the updated state of the chess board after
#'   executing the move.
#'
#' @examples
#' # Initialize a chess board
#' position <- position.start()
#' # Execute the move e4
#' updated_position <- position.move(position, r1=7, c1=5, r2=5, c2=5)
#'
#' @export
position.move <- function(position, r1, c1, r2, c2, p = NA) {
  # Identify pieces at source and destination
  p1 <- position[r1, c1]
  p2 <- position[r2, c2]

  # Move piece from source to destination
  position[r2, c2] <- position[r1, c1]
  position[r1, c1] <- 0

  # Check for castling move
  if (abs(p1) == 6 & abs(c1 - c2) > 1) {
    # Short castling
    if (c2 == 7) {
      # Move rook for short castling
      position[r1, 6] <- position[r1, 8]
      position[r1, 8] <- 0
    }

    # Long castling
    if (c2 == 3) {
      # Move rook for long castling
      position[r1, 4] <- position[r1, 1]
      position[r1, 1] <- 0
    }
  }

  # Check for en passant move
  if (abs(p1) == 1 & abs(c1 - c2) > 0 & p2 == 0) {
    # Remove opponent's pawn for en passant
    position[r1, c2] <-0
  }

  # Check for promotion
  if (!is.na(p)) {
    # Change pawn to specified piece for promotion
    position[r2, c2] <- p * p1
  }

  # Return updated board state after move
  return(position)
}
