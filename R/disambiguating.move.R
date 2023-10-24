#' Disambiguate chess moves
#'
#' When two or more identical chess pieces are able to move to the same square,
#' chess notation must somehow distinguish which piece was moved. In standard
#' algebraic notation (SAN), the ambiguity is resolved by adding row or column
#' names as needed to specify which piece moved. This function generates the
#' string of row or column names needed to specify the move in SAN.
#'
#' @details The function evaluates the current state of the game and the
#'   positions of the pieces before and after the move. It then generates a
#'   string that uniquely identifies which piece made the move.
#'
#' @param position A matrix representing the current position on the chess
#'   board.
#' @param r1 An integer representing the row of the piece before the move.
#' @param c1 An integer representing the column of the piece before the move.
#' @param r2 An integer representing the row of the piece after the move.
#' @param c2 An integer representing the column of the piece after the move.
#'
#' @return A string that disambiguates the move. If no disambiguation is needed,
#'   it returns an empty string.
#'
#' @export
#'
#' @examples
#' # Initialize a chess board
#' position <- position.start()
#' # Add a white knight at e3
#' position[5, 5] <- 3
#' # Disambiguate a knight move from b1 (8, 2) to c3 (6, 3)
#' disambiguating.move(position, 8, 2, 6, 3)
disambiguating.move <- function(position, r1, c1, r2, c2) {
  # Extracts the piece from the current position
  p1 <- position[r1, c1]

  # Initializes variables for different cases of disambiguation
  tsr <- FALSE # The same rank
  tsf <- FALSE # The same file
  oth <- FALSE # Other cases

  # Initializes an empty string for disambiguation
  dsmb <- ""

  # Checks if the piece is a rook
  if(abs(p1) == 4) {
    # Finds all pieces identical to p1 on the board
    wpp <- which(position == p1, arr.ind = TRUE)

    # Removes the current piece from wpp
    wppp <- matrix(wpp[!(wpp[, 1] == r1 & wpp[, 2] == c1), ], ncol = 2)

    if(nrow(wppp) > 0) {
      for(ff in 1:nrow(wppp)) {
        dr <- wppp[ff, ]
        if(possible.move(position, dr[1], dr[2], r2, c2)) {
          if(dr[1] == r1) {
            dsmb <- letters[c1]
            break()
          } else {
            dsmb <- as.character(9 - r1)
          }
        }
      }
    }
  }

  # Checks if the piece is a bishop, knight or queen
  if(abs(p1) > 1 & abs(p1) < 6) {
    wpp <- which(position == p1, arr.ind = TRUE)
    wppp <- matrix(wpp[!(wpp[, 1] == r1 & wpp[, 2] == c1), ], ncol = 2)

    if(nrow(wppp) > 0) {
      for(ff in 1:nrow(wppp)) {
        dr <- wppp[ff, ]
        if(possible.move(position, dr[1], dr[2], r2, c2)) {
          if(dr[1] == r1) tsr <- TRUE
          else if(dr[2] == c1) tsf <- TRUE
          else oth <- TRUE
        }
      }

      if(tsr & tsf) dsmb <- paste0(letters[c1],9 - r1)
      else if(tsr & !tsf) dsmb <- letters[c1]
      else if(tsf) dsmb <- as.character(9 - r1)
      else if(oth) dsmb <- letters[c1]
    }
  }

  return(dsmb)
}
