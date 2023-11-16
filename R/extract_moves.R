#' Extract the first N moves
#'
#' This function takes a PGN movetext string (or string vector) and extracts the
#' first N moves into a data frame.
#'
#' @details The function requires the standard English piece notations: pawn =
#'   "P" (often not used), knight = "N", bishop = "B", rook = "R", queen = "Q",
#'   and king = "K". It returns a data frame containing the first N moves for
#'   white and for black, named as W1, B1, W2 and so on, up to WN and BN (where
#'   N is the input argument). If N is greater than the total moves number then
#'   NA's are generated.
#'
#' @param movetext A character vector of movetext.
#' @param N (default = 10) An integer that determines how many moves will be
#'   extracted. Must be greater than 0.
#' @param last.move (default TRUE) A Boolean indicating whether to calculate the
#'   last move.
#'
#' @return A data frame containing the first N moves for white and for black.
#'
#' @examples
#' extract_moves("1. e4 e5 2. Nf3 Nf5 3. d5 ",N = 3)
#' # e4 e5 Nf3 Nf5 d5 NA TRUE
#' extract_moves("1. e4 e5 2. Nf3 Nf5 3. d5 ",N = 3, last.move = TRUE)
#' # e4 e5 Nf3 Nf5 d5 NA d5 TRUE
#'
#' @export
extract_moves <- function(movetext, N = 10, last.move = T) {
  # Remove move numbers, game results, comments, and extra spaces from movetext
  x <- gsub("([0-9+]{0,}\\.)", "", movetext, perl = T)
  x <- gsub("1-0", "", x, perl = T)
  x <- gsub("0-1", "", x, perl = T)
  x <- gsub("1/2-1/2", "", x, perl = T)
  x <- gsub("\\*", "", x, perl = T)
  x <- gsub("{[^}]+}", "", x, perl = T)

  # Remove extra spaces and split cleaned movetext into individual moves
  x <- trimws(gsub(" {2}", " ", x))
  sx <- strsplit(x, " ")

  # Extract first N*2 moves from each game and create a data frame
  r <- data.frame(
    t(sapply(sx, function(i) {
      return(i[1:(N * 2)])
    })),
    stringsAsFactors = TRUE
  )

  # Name columns as W1, B1, W2, ..., WN, BN
  colnames(r) <- paste0(rep(c("W", "B"), times = N), rep(1:N, each = 2))

  # Remove row names
  rownames(r) <- NULL

  # Check if movetext starts with "1."
  cmplt <- substr(trimws(gsub(" {2}", " ", movetext)), 1, 3)

  # If last.move is TRUE, add last move to data frame
  if (last.move) {
    r$last.move <- vapply(
      sx,
      function(i) {
        if (length(i) > 1) return(tail(i, n = 1)) else return("")
      },
      character(1)
    )
  }

  # Add complete.movetext flag to data frame
  r$complete.movetext <- substr(cmplt, 1, 2) == "1." & substr(cmplt, 2, 3) != ".."

  # If complete.movetext is FALSE, set all other columns to NA
  r[!r$complete.movetext,-ncol(r)] <- NA

  return(r)
}
