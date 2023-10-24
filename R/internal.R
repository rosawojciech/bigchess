#' Compute possible moves for a generic chess piece along a single specified
#' rank, file, or diagonal
#'
#' The `internal()` function calculates all possible moves for a generic chess
#' piece at position `xp` within a specific row, column, or diagonal
#' (represented by `vec`) on the chessboard. It assumes that the piece can move
#' like any piece (pawn, rook, knight, bishop, queen, king).
#'
#' @details `internal()` is used by the `position.moves()` function. It
#'   considers the positions of other pieces in the same row, column, or
#'   diagonal to determine where the piece at position `xp` can move. The
#'   function returns a vector of indices representing the possible moves for
#'   the piece.
#'
#' @param vec A vector representing a row, column, or diagonal on the
#'   chessboard.
#' @param xp The index of the piece in `vec`.
#'
#' @return A vector of indices in `vec` representing all possible moves for a
#'   generic piece.
#'
#' @examples
#' # Compute possible moves for a generic piece at index 4 in a vector
#' internal(c(0, 0, 0, 1, 0, 0, 0), 4)
#'
#' @export
internal <- function(vec, xp) {
  # Initialize result vector
  rslt <- NULL

  # Compute possible moves to the right
  if (xp < length(vec)) {
    wvx <- which(vec[(xp + 1) : length(vec)] != 0)

    # If there are other pieces
    if (length(wvx) > 0) {
      mwvx <- min(wvx)

      # If this piece could not be taken then remove it from possible moves
      if (vec[xp + mwvx] * vec[xp] > 0) {
        mwvx <- mwvx - 1
      }

      # Add possible moves to the result
      if (mwvx > 0) {
        rslt <- c(rslt, (xp + 1) : (xp + mwvx))
      }
    } else {
      rslt <- c(rslt, (xp + 1) : length(vec))
    }
  }

  # Compute possible moves to the left
  if (xp > 1){
    wvx <- which(vec[1 : (xp - 1)] != 0)

    # If there are other pieces
    if (length(wvx) > 0) {
      mwvx <- max(wvx)

      # If this piece could not be taken then remove it from possible moves
      if (vec[mwvx] * vec[xp] > 0) {
        mwvx <- mwvx + 1
      }

      # Add possible moves to the result
      if (mwvx < xp) {
        rslt <- c(rslt, mwvx : (xp - 1))
      }
    } else {
      rslt <- c(rslt, 1 : (xp - 1))
    }
  }

  return(rslt)
}
