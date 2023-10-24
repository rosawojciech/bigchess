#' Convert LAN movetext to SAN
#'
#' @description This function converts UCI LAN movetext (long algebraic notation
#'   used by [UCI-compatible](https://wbec-ridderkerk.nl/html/UCIProtocol.html)
#'   chess engines) to SAN movetext (standard algebraic notation).
#'
#' @details The `movetext.lan` argument should be a string of chess moves in UCI
#'   long algebraic notation that is free of any comments, annotations, etc. The
#'   function returns a new string with the same moves converted to standard
#'   algebraic notation.
#'
#' @param movetext.lan A string of movetext in UCI long algebraic notation
#'   (LAN).
#'
#' @return A string representing the movetext in standard algebraic notation.
#'
#' @examples
#' lan2san("e2e4 c7c5")
#'
#' @export
lan2san <- function(movetext.lan) {
  # Split and trim the input movetext
  tmpr <- strsplit(trimws(movetext.lan), " ")[[1]]

  # Initialize variables
  cursan <- ""
  position <- position.start()

  # Loop through each move in the input movetext
  for (ft in seq_along(tmpr)) {
    # Parse LAN move
    slm <- string.lan.move2move(tmpr[ft])

    # Add move number for white's turn
    if (ft %% 2) {
      cursan <- paste0(cursan, round((ft + 1) / 2), ". ")
    }

    # Extract move details
    r1 <- slm[1]
    c1 <- slm[2]
    r2 <- slm[3]
    c2 <- slm[4]
    p  <- slm[5]

    # Get piece at source square
    p1 <- position[r1, c1]

    # Convert LAN move to SAN and append to current SAN movetext
    cursan <- paste0(cursan, move2san(position, r1, c1, r2, c2, p))

    # Update position after making the move
    position <- position.move(position, r1, c1, r2, c2, p)

    # Check for check or checkmate and append appropriate symbol to SAN movetext
    if (is.check(position, sign(p1))) {
      if (is.mate(position, sign(p1))) {
        cursan <- paste0(cursan, "# ")
      } else {
        cursan <- paste0(cursan, "+ ")
      }
    } else {
      cursan <- paste0(cursan, " ")
    }
  }

  # Return the final SAN movetext after trimming any leading/trailing whitespace
  return(trimws(cursan))
}
