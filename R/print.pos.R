#' Print a chessboard
#'
#' This function prints a chessboard to the console.
#'
#' @details The function accepts an 8 x 8 matrix representing a chess position
#'   as its argument. Each cell corresponds to a square, with numeric codes for
#'   each piece, and with the sign indicating color (- for black): 0 = empty, 1
#'   = pawn, 2 = bishop, 3 = knight, 4 = rook, 5 = queen, 6 = king. The pieces
#'   are printed using Unicode.
#'
#' @param position An 8 x 8 numeric matrix of a chessboard.
#'
#' @return `NULL`
#'
#' @examples
#' # Initialize a chessboard
#' position <- position.start()
#' # Print the chessboard
#' print.pos(position)
#'
#' @rawNamespace export(print.pos)

# Note: @export mistakes print.pos() for method of base::print().
# @rawNamespace is used in place of @export to force print.pos() to be exported.
print.pos <- function(position) {
  # Set up Unicode for chess pieces
  white_ucode <- c('\U2659', '\U2657', '\U2658', '\U2656', '\U2655', '\U2654')
  black_ucode <- c('\U265F', '\U265D', '\U265E', '\U265C', '\U265B', '\U265A')

  # Loop over each row of the board
  for (fr in 1:8) {
    rslt <- NULL

    # Loop over each column of the board
    for (fc in 1:8) {
      pff <- position[fr, fc]

      # Get Unicode
      if (pff > 0) tmp <- white_ucode[pff]
      if (pff < 0) tmp <- black_ucode[-pff]
      # Empty square Unicode
      if (pff == 0) tmp <- '\U26DD'

      # Add piece to current row
      rslt <- paste0(rslt, tmp)
    }

    # Print current row to console
    cat(paste0(rslt, '\n'))
  }
}
