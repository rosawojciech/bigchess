#' Count the number of moves
#'
#' This function counts the moves in a given movetext.
#'
#' @details  The `movetext` argument is a character vector and may contain
#'   multiple movetext strings. The function will return an integer vector of
#'   equal length to the input containing the move count for each entry.
#'
#' @param movetext A character vector of movetext strings.
#'
#' @return An integer vector of move numbers.
#'
#' @examples
#' n_moves(c("1. e4 e5 2. Nf3 Nf5 3. d5 ", "1. d4 d5"))
#'
#' @export
n_moves <- function(movetext) {
  # Define regex pattern for move numbers
  pattern <- "([0-9+]{0,}\\.)"

  # Extract move numbers from movetext using regex pattern
  move_numbers <- regmatches(movetext, gregexpr(pattern, movetext))

  # Compute and return the total number of moves
  return(unlist(lapply(move_numbers, function(i) {
    max(as.integer(gsub("\\.", "", tail(i, 1))), 0)
  })))
}
