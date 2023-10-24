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
  rslt <- FALSE # Initialize result as FALSE

  # Get the position of the king on the board
  kp <- which(position == -p * 6, arr.ind = TRUE)
  kp1 <- kp[, 1]
  kp2 <- kp[, 2]

  # Loop through all squares on the board
  for(fr in 1:8) {
    for(fc in 1:8) {
      # If there's a piece of the same color on this square
      if(position[fr, fc] * p > 0) {
        # Get all possible moves for this piece
        pm <- checking.moves(position, fr, fc)
        # If there are any possible moves
        if(nrow(pm) > 0) {
          # If any of these moves could capture the king
          if(sum(pm[, 1] == kp1 & pm[, 2] == kp2) > 0) {
            rslt <- TRUE
          }
        }
      }
      # If we've found a piece that can capture the king, break out of loop
      if(rslt) break()
    }
    # If we've found a piece that can capture the king, break out of loop
    if(rslt) break()
  }

  return(rslt)
}
