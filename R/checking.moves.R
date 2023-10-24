#' Determine possible moves for a chess piece
#'
#' This function calculates the potential moves for a specific chess piece based
#' on its current position on the board.
#'
#' @details The function takes into account the type of piece (e.g., rook,
#'   queen, bishop, knight, pawn, king) and its current position to determine
#'   all possible destination squares.
#'
#' @param position A matrix representing the current state of the chess board.
#' @param r1 The row index of the piece for which to check possible moves.
#' @param c1 The column index of the piece for which to check possible moves.
#'
#' @return A matrix where each row represents a possible destination square for
#'   the piece. The first column stores the row indices and the second column
#'   stores the column indices of these squares.
#'
#' @export
#'
#' @examples
#' # Determine possible moves for the white knight on b1 at the start of a game.
#' position <- position.start()
#' checking.moves(position = position, r1 = 1, c1 = 2)
checking.moves <- function(position, r1, c1) {
  p1 <- position[r1, c1]       # Get the piece at the specified position
  ap1 <- abs(p1)              # Get the absolute value of the piece
  pm <- matrix(ncol = 2)[-1, ] # Initialize the potential moves matrix

  # Check for rook and queen
  if (ap1 %in% c(4, 5)) {
    # Check internal moves in the same row
    ipr1 <- internal(position[r1, ], c1)
    if (length(ipr1) > 0) pm <- rbind(pm, cbind(r1, ipr1))

    # Check internal moves in the same column
    ipc1 <- internal(position[, c1], r1)
    if (length(ipc1) > 0) pm <- rbind(pm, cbind(ipc1, c1))
  }

  # Check for bishop and queen
  if (ap1 %in% c(2, 5)) {
    d1rc <- diag1[r1, c1]
    spd1 <- split(position, diag1)[[as.character(d1rc)]]
    if (length(spd1) > 1) {
      md1rc <- min(d1rc, 0)
      is1 <- internal(spd1, c1 + md1rc)
      if (length(is1) > 0)
        pm <- rbind(pm, cbind(r1 + is1 - c1 - md1rc, is1 - md1rc))
    }

    d2rc <- diag2[r1, c1]
    spd2 <- split(position, diag2)[[as.character(diag2[r1, c1])]]
    if (length(spd2) > 1) {
      md2rc <- min(d2rc, 0)
      is2 <- internal(spd2, c1 + md2rc)
      if (length(is2) > 0)
        pm <- rbind(pm, cbind(r1 - is2 + c1 + md2rc, is2 - md2rc))
    }
  }

  # Check for knight
  if (ap1 == 3) {
    pm <- kni
    pm[, 1] <- pm[, 1] + r1
    pm[, 2] <- pm[, 2] + c1
    pm <- pm[pm[, 1] > 0 & pm[, 1] < 9 & pm[, 2] > 0 & pm[, 2] < 9, ]
    pm <- pm[position[pm] * p1 <= 0, ]
  }

  # Check for pawn
  if (ap1 == 1) {
    # Typical pawn move
    if (position[r1 - p1, c1] == 0) pm <- rbind(pm, c(r1 - p1, c1))

    # First doubled move
    if ((p1 == 1 & r1 == 7) || (p1 == -1 & r1 == 2))
      if (position[r1 - 2 * p1, c1] == 0 & position[r1 - p1, c1] == 0)
        pm <- rbind(pm, c(r1 - 2 * p1, c1))

    # Capturing moves
    if (c1 > 1)
      if (position[r1 - p1, c1 - 1] * p1 < 0)
        pm <- rbind(pm, c(r1 - p1, c1 - 1))
    if (c1 < 8)
      if (position[r1 - p1, c1 + 1] * p1 < 0)
        pm <- rbind(pm, c(r1 - p1, c1 + 1))
  }

  # Check for king
  if (ap1 == 6) {
    pm <- kin
    pm[, 1] <- pm[, 1] + r1
    pm[, 2] <- pm[, 2] + c1
    pm <- pm[pm[, 1] > 0 & pm[, 1] < 9 & pm[, 2] > 0 & pm[, 2] < 9, ]
    pm <- pm[position[pm] * p1 <= 0, ]
  }

  return(matrix(pm, ncol = 2)) # Return the potential moves matrix
}
