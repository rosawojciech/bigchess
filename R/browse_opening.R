#' Browse Opening
#'
#' This function allows you to browse opening winning and drawing percentages by
#' table and barplot.
#'
#' @details The function takes a data frame of chess games and a string
#'   indicating the move text. It returns a data frame from the tree_move
#'   function and a plot from the plot_tree_move function.
#'
#'   The default movetext argument is "", which is just the starting position.
#'   The standard English piece notations are required: pawn = "P" (often not
#'   used), knight = "N", bishop = "B", rook = "R", queen = "Q", and king = "K".
#'
#' @param df A data frame with chess games imported using read.pgn().
#' @param movetext (default = "") A string indicating the move text.
#'
#' @return A data frame from the tree_move function and a plot from the
#'   plot_tree_move function.
#'
#' @examples
#' f <- system.file("extdata", "Kasparov.gz", package = "bigchess")
#' con <- gzfile(f, encoding = "latin1")
#' df <- read.pgn(con, quiet = TRUE, ignore.other.games = TRUE,
#'                stat.moves = FALSE)
#' # Analyze best Kasparov openings:
#' bo <- browse_opening(subset(df, grepl("Kasparov", White)))
#' # Analyze 'best' answer to Kasparov Ruy Lopez:
#' bo <- browse_opening(subset(df, grepl("Kasparov", White)),
#'                      "1.e4 e5 2.Nf3 Nc6 3.Bb5")
#' # Analyze best answer to "1.e4 e5 2.Nf3" in aggregated data
#' browse_opening(FirstTwoMoves, "1.e4 e5 2.Nf3")
#'
#' @export
browse_opening <- function(df, movetext = "") {
  # Initialize opening with movetext
  opening <- movetext

  if (opening == "") {
    r <- df
    move <- "W1"
  } else {
    # Get number of moves in opening
    no <- n_moves(opening)

    # Extract moves from opening
    emo <- data.frame(extract_moves(opening, no, F), stringsAsFactors = T)

    ncemo <- ncol(emo) - 1

    if (is.na(emo[, ncemo])) {
      r <- merge(df, emo[, -ncemo])
      move <- colnames(emo)[ncemo]
    } else {
      r <- merge(df, emo)
      move <- paste0("W", ncemo / 2 + 1)
    }
  }

  if (nrow(r) > 0) {
    # Call tree_move function on r and move
    tr <- tree_move(r, move)

    # Plot the result with a title showing total number of games
    plot_tree_move(tr, main = paste(opening, "\n N =", sum(tr$N)))

    # Return the result from tree_move
    return(tr)
  } else {
    message("No openings found in data frame")

    # Return an empty data frame if no openings found
    return(data.frame())
  }
}
