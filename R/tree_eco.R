utils::globalVariables(c("Result"))
#' Compute an ECO tree
#'
#' This function creates a data frame of the outcomes of a set of chess games.
#'
#' @details This function takes a data frame with 'ECO' and 'Result' columns,
#'   where 'ECO' represents the opening code and 'Result' denotes the game
#'   result ('1-0' for White win, '1/2-1/2' for draw, '0-1' for Black win). The
#'   function aggregates the data to calculate the winning percentages and
#'   frequencies for each opening code.
#'
#' @param df A data frame containing the 'ECO' and 'Result' columns.
#' @param topn (default = 0) An integer indicating how many openings to include.
#'   Set to 0 to include all openings.
#'
#' @return A data frame containing 'White_score' (White's winning percentage),
#'   'Draws_percent' (draw percentage), 'Black_score' (Black's winning
#'   percentage), and 'N' (number of games). The data frame is sorted by the
#'   power of ECO (White_score * N), descending.
#'
#' @importFrom stats aggregate as.formula
#'
#' @examples
#' f <- system.file("extdata", "Kasparov.gz", package = "bigchess")
#' con <- gzfile(f, encoding = "latin1")
#' df <- read.pgn(con, quiet = TRUE, stat.moves = FALSE, add.tags = "ECO")
#' # Get the top 10 openings, by winning percentages
#' result <- tree_eco(df, topn = 10)
#'
#' @export
tree_eco <- function(df, topn = 0) {
  # If there is no "Freq" column in the dataframe, create one and set all its
  # values to 1
  if (!("Freq" %in% colnames(df))) {
    df$Freq <- 1
  }

  # Aggregate the frequency of each Result and ECO combination
  t <- aggregate(as.formula(paste("Freq ~ Result + ECO")), df, FUN = sum)

  # Subset the dataframe for games where White won ("1-0")
  r <- subset(t, Result == "1-0")

  # Merge the dataframe with subsets for games that resulted in a draw
  # ("1/2-1/2") or Black won ("0-1")
  for (i in c("1/2-1/2", "0-1")) {
    r <- merge(r, subset(t, Result == i), by = "ECO", all = T)
  }

  # Select only the ECO column and the frequency columns from the merged
  # dataframe
  r <- r[, c(1, 3, 5, 7)]

  # Replace any NA values with 0
  r[is.na(r)] <- 0

  # Calculate the total number of games for each ECO code
  r$N <- rowSums(r[, -1])

  # Calculate the percentage of games that resulted in White win, draw, or Black
  # win for each ECO code
  r[, 2:4] <- round(r[, 2:4] / r$N * 100, 1)

  # Rename the columns appropriately
  colnames(r) <- c("ECO", "White_score", "Draws_percent", "Black_score", "N")

  # Sort the dataframe by the power of ECO (White_score * N) in descending order
  r <- r[order(r$White_score * r$N, decreasing = T), ]

  # If topn is greater than zero, select only the topn rows from the sorted
  # dataframe
  if (topn > 0) {
    r <- r[1:topn, ]
  }

  return(r)
}
