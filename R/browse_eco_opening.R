#' Browse ECO Opening
#'
#' This function allows you to browse ECO opening winning and drawing
#' percentages by table and barplot.
#'
#' @details The function takes a data frame of chess games and an integer
#'   indicating the number of top openings to include. It returns a data frame
#'   from the tree_eco function and a plot from the plot_tree_eco function.
#'
#' @param df A data frame with chess games imported using read.pgn().
#' @param topn (default = 0) An integer indicating how many top openings to
#'   include.
#'
#' @return A data frame from the tree_eco function and a plot from the
#'   plot_tree_eco function.
#'
#' @examples
#' f <- system.file("extdata", "Kasparov.gz", package = "bigchess")
#' con <- gzfile(f, encoding = "latin1")
#' df <- read.pgn(con, quiet = TRUE, ignore.other.games = TRUE,
#'                stat.moves = FALSE, add.tags = "ECO")
#' # Analyze 20 best ECO Kasparov openings:
#' bo <- browse_eco_opening(subset(df, grepl("Kasparov", White)), 20)
#'
#' @export
browse_eco_opening <- function(df, topn = 0) {
  # Call tree_eco function on df and topn
  tr <- tree_eco(df, topn)

  # Plot the result with a title showing total number of games
  plot_tree_move(tr, main = paste("N =", sum(tr$N)))

  # Return the result from tree_eco
  return(tr)
}
