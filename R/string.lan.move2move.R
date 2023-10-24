#' Convert LAN to chessboard coordinates
#'
#' This function translates a chess move expressed in long algebraic notation
#' (LAN) into chessboard coordinates.
#'
#' @details In this function, each square of the chessboard is identified by a
#'   unique coordinate pair of integers. The vertical columns of squares, from
#'   White's left to right, are labeled 1 through 8. The horizontal rows of
#'   squares, from White's perspective, are also numbered 1 through 8 bottom to
#'   top.
#'
#'   This function takes a move in LAN (e.g., "e7e8q") and translates it into
#'   integer coordinates. If the move involves a pawn promotion, the function
#'   also identifies the new piece.
#'
#' @param slm A string representing a move in LAN. The string should be in the
#'   format "e7e8q" where "e7" is the current position, "e8" is the destination,
#'   and "q" (optional) is the piece to be promoted to.
#'
#' @return A vector containing the rank and file of the piece's current position
#'   and destination, and the piece to be promoted to (if applicable). The
#'   vector is in the format c(current_rank, current_file, destination_rank,
#'   destination_file, promotion_piece).
#'
#' @examples
#' # Convert a LAN move to coordinates
#' string.lan.move2move("e7e8q")
#'
#' @export
string.lan.move2move <- function(slm) {
  # Process the input string
  lslm <- strsplit(slm, "")[[1]]
  rslm <- NULL

  # Translate characters into chessboard coordinates
  rslm[2] <- cn[lslm[1]]
  rslm[1] <- rn[lslm[2]]
  rslm[4] <- cn[lslm[3]]
  rslm[3] <- rn[lslm[4]]

  # Identify the new piece if a pawn promotion is involved
  rslm[5] <- figs[casefold(lslm[5])]

  # Return the chessboard coordinates
  return(rslm)
}
