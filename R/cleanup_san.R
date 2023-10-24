#' Clean up standard algebraic notation (SAN)
#'
#' This function removes move numbers, check, checkmate, and capture symbols.
#'
#' @details The function takes a string of SAN as input and removes any move
#'   numbers (1.), check (+), checkmate (#), or capture (x) symbols.
#'
#' @param movetext.san A string representing a move in SAN.
#'
#' @return A string representing the cleaned up move in SAN.
#'
#' @export
#'
#' @examples
#' # Clean up a SAN move
#' cleanup_san("1.e4 e5 2.Nf3")
cleanup_san <- function(movetext.san) {
  # Remove move numbers
  rslt <- gsub("[0-9]+\\.", "", movetext.san)

  # Remove extra spaces
  rslt <- gsub("  ", " ", rslt)

  # Remove check, checkmate, and capture symbols
  rslt <- gsub("[#x\\+]", "", rslt)

  # Return the cleaned up SAN move
  return(trimws(rslt))
}
