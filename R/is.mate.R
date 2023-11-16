#' Check if a king is in checkmate
#'
#' This function determines whether a specified king is in checkmate. It does
#' this by first checking if the king is in check, and then verifying if there
#' are no legal moves left for the king.
#'
#' @details The function takes into account the current state of the game and
#'   the color of the king in question. It then checks all possible moves of the
#'   king's pieces to see if any could prevent a checkmate.
#'
#' @param position A matrix representing the current state of the chess board.
#' @param p The color of the king to check (1 for white, -1 for black).
#'
#' @return A Boolean value indicating whether the king is in checkmate (TRUE) or
#'   not (FALSE).
#'
#' @export
#'
#' @examples
#' # Initialize a chess board
#' position <- position.start()
#' # Check if the white king is in checkmate
#' is.mate(position, 1)
is.mate <- function(position, p) {
  rslt <- TRUE

  # Loop through all squares on the board
  for(fr in 1:8) {
    for(fc in 1:8) {
      # If there's a piece of the same color on this square
      if(position[fr, fc] * p < 0) {
        # Get all possible moves for this piece
        pm <- possible.moves(position, fr, fc)
        # If there are any possible moves
        if(nrow(pm) > 0) {
          rslt <- FALSE
        }
      }
      # If we've found a piece that can prevent a checkmate, break out of loop
      if(!rslt) break()
    }
    # If we've found a piece that can prevent a checkmate, break out of loop
    if(!rslt) break()
  }

  return(rslt)
}
