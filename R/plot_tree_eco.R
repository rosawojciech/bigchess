#' Plot the tree for an ECO tree table
#'
#' This function creates a bar plot of the outcomes of a set of chess games.
#'
#' @details The function takes a data frame of ECO tree data from `tree_eco()`
#'   as an argument. The bar plot shows the percentage of games that resulted in
#'   a win, loss, or draw. The weighted mean of the percentages is also shown.
#'
#' @param tr A data frame containing ECO tree data.
#' @param main (default = "") A string for the main title.
#' @param add.lines (default = TRUE) A Boolean indicating whether to add
#'   weighted mean lines.
#' @param add.labels (default = TRUE) A Boolean indicating whether to add
#'   labels.
#'
#' @return A barplot of the percentage of wins, losses, and draws.
#'
#' @examples
#' f <- system.file("extdata", "Kasparov.gz", package = "bigchess")
#' con <- gzfile(f, encoding = "latin1")
#' df <- read.pgn(con, quiet = TRUE, stat.moves = FALSE, add.tags = "ECO")
#' tr <- tree_eco(subset(df, W1 == "e4"), 20)
#' plot_tree_eco(tr, "1. e4 ... ?")
#'
#' @importFrom graphics barplot lines text
#' @export
plot_tree_eco <- function(tr, main = "", add.lines = TRUE, add.labels = TRUE) {
  # Reverse order of rows in tr for plotting
  rtr <- tr[rev(seq_len(nrow(tr))), ]

  # Create barplot
  ptr <- barplot(
    as.table(t(rtr[, c("White_score", "Draws_percent", "Black_score")])),
    horiz = TRUE,
    names.arg = paste0("(", rtr[, "N"], ") ", rtr[, 1]),
    las = 1,
    col = c("White", "grey", "black"),
    cex.names = .8,
    main = main
  )

  # Add weighted mean lines if requested
  if (add.lines) {
    lines(rep(sum(tr$White_score * tr$N) / sum(tr$N), nrow(tr)),
          ptr, col = "black")
    lines(rep(100 - sum(tr$Black_score * tr$N) / sum(tr$N), nrow(tr)),
          ptr, col = "white")
  }

  # Add labels if requested
  if (add.labels) {
    prws <- paste0(rev(format(tr$White_score, nsmall = 1)), "%")
    prws[rev(tr$White_score) == 0] <- " "
    text(x = 5, y = ptr, labels = prws)

    prbs <- paste0(rev(format(tr$Black_score, nsmall = 1)), "%")
    prbs[rev(tr$Black_score) == 0] <- " "
    text(x = 95, y = ptr, labels = prbs, col = "White")

    prdp <- paste0(rev(format(tr$Draws_percent, nsmall = 1)), "%")
    prdp[rev(tr$Draws_percent) == 0] <- " "
    text(x = rev(tr$White_score + tr$Draws_percent / 2),
         y = ptr, labels = prdp, col = "Black")
  }
}
