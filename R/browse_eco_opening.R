#' Browse ECO opening
#'
#' Browse ECO opening winning and drawing percentages by table and barplot
#'
#' @param df data frame with imported chess games from read.pgn() function.
#' @param topn integer, default is 0, passed to tree_eco function (indicating how many top openings should be included).
#' @return Data frame from tree_eco function and plot from plot_tree_eco function.
#'
#' @examples
#' f <- system.file("extdata", "Kasparov.gz", package = "bigchess")
#' con <- gzfile(f,encoding = "latin1")
#' df <- read.pgn(con,quiet = TRUE,ignore.other.games = TRUE,stat.moves = FALSE, add.tags = "ECO")
#' # Analyze 20 best ECO Kasparov openings:
#' bo <- browse_eco_opening(subset(df,grepl("Kasparov",White)),20)
#' @export
browse_eco_opening <- function(df,topn = 0){

  tr <- tree_eco(df,topn)
  plot_tree_move(tr,main = paste("N = ",sum(tr$N)))
  return(tr)
}
