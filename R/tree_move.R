# Declare global variables
utils::globalVariables(c("Result"))

#' Compute a Move Tree
#'
#' This function computes a tree for a given chess move, calculating the
#' frequencies and winning percentages.
#'
#' @details The function takes a data frame with a 'Move' and 'Result' column,
#'   where 'Move' represents a specific move in the game and 'Result' denotes
#'   the game result ('1-0' for White win, '1/2-1/2' for draw, '0-1' for Black
#'   win). The function aggregates the data to calculate the winning percentages
#'   and frequencies for each move.
#'
#' @param df A data frame containing the 'Move' and 'Result' columns. This can
#'   either be from the pgn function or an aggregated data frame from such df
#'   (containing columns: Result, W1, B1, W2, ..., WN, BN, Freq).
#' @param move A character string indicating which move should be analyzed. For
#'   example, "W1".
#'
#' @return A data frame containing 'White_score' (White's winning percentage),
#'   'Draws_percent' (draw percentage), 'Black_score' (Black's winning
#'   percentage), and 'N' (number of games). The data frame is sorted by the
#'   power of the move (White_score * N), which describes the popularity and
#'   score of the move, in descending order.
#'
#' @importFrom stats aggregate as.formula
#'
#' @examples
#' f <- system.file("extdata", "Kasparov.gz", package = "bigchess")
#' con <- gzfile(f, encoding = "latin1")
#' df <- read.pgn(con, quiet = TRUE, stat.moves = FALSE)
#' # Analyze best responses to 1. e4 in Kasparov games (both white and black)
#' tree_move(subset(df,W1=="e4"),move = "B1")
#' # Analyze openings in aggregated data
#' tree_move(FirstTwoMoves,"W1")
#'
#' @export
tree_move <- function(df, move) {
  # Drop unused levels in the specified move column
  df[, move] <- droplevels(df[, move])

  # If there is no "Freq" column in the dataframe, create one and set all its
  # values to 1
  if (!("Freq" %in% colnames(df))) {
    df$Freq <- 1
  }

  # Aggregate the frequency of each Result and specified move combination
  t <- aggregate(as.formula(paste("Freq ~ Result +", move)), df, FUN = sum)

  # Subset the dataframe for games where White won ("1-0")
  r <- subset(t, Result == "1-0")

  # Merge the dataframe with subsets for games that resulted in a draw
  # ("1/2-1/2") or Black won ("0-1")
  for (i in c("1/2-1/2", "0-1")) {
    r <- merge(r, subset(t, Result == i), by = move, all = T)
  }

  # Select only the move column and the frequency columns from the merged
  # dataframe
  r <- r[, c(1, 3, 5, 7)]

  # Replace any NA values with 0
  r[is.na(r)] <- 0

  # Calculate the total number of games for each specified move
  r$N <- rowSums(r[, -1])

  # Calculate the percentage of games that resulted in White win, draw, or Black
  # win for each specified move
  r[,2:4] <- round(r[,2:4] / r$N * 100, 1)

  # Rename the columns appropriately
  colnames(r) <- c(move,"White_score","Draws_percent","Black_score","N")

  # Sort the dataframe by the power of the specified move (White_score * N) in
  # descending order
  r <- r[order(r$White_score * r$N, decreasing = T), ]

  return(r)
}
