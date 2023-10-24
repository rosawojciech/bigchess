utils::globalVariables(c("White", "Black"))

#' Compute a player's move frequency profile
#'
#' This function shows how often a player moved each type of chess piece.
#'
#' @details The function accepts a data frame of chess games typically obtained
#'   using `read.pgn()` or `read.pgn.ff()`. The function returns a data frame
#'   where each row corresponds to a game, and move counts for each type of
#'   piece are arranged by column.
#'
#' @param df A data frame of chess games.
#' @param player A string specifying the name of the player.
#'
#' @return A data frame where each row represents a game, and move counts for
#'   each type of piece are arranged by column.
#'
#' @examples
#' # Generate a profile for Kasparov
#' f <- system.file("extdata", "Kasparov.gz", package = "bigchess")
#' con <- gzfile(f, encoding = "latin1")
#' df <- read.pgn(con, quiet = TRUE, ignore.other.games = TRUE)
#' df_pp <- player_profile(df, "Kasparov")
#'
#' # Generate a profile for Carlsen
#' f <- system.file("extdata", "Carlsen.gz", package = "bigchess")
#' con <- gzfile(f, encoding = "latin1")
#' df <- read.pgn(con, quiet = TRUE, ignore.other.games = TRUE)
#' df_pp <- player_profile(df, "Carlsen")
#'
#' @export
player_profile <- function(df, player) {
  # Define figure types
  fg <- c("B", "K", "N", "O", "Q", "R")

  # Subset data for games where the player played as White
  dw <- subset(df, grepl(player, White))
  if (nrow(dw) > 0) {
    # Create a new data frame with player and opponent move counts
    rdw <- dw[, c(paste0("W_", fg, "_moves"), paste0("B_", fg, "_moves"))]
    colnames(rdw) <- c(paste0("P_", fg, "_moves"), paste0("O_", fg, "_moves"))
    rdw$Player_Col <- "White"
    rrdw <- cbind(dw, rdw)
  } else {
    rrdw <- data.frame()
  }

  # Subset data for games where the player played as Black
  db <- subset(df, grepl(player, Black))
  if (nrow(db) > 0) {
    # Create a new data frame with player and opponent move counts
    rdb <- db[, c(paste0("B_", fg, "_moves"), paste0("W_", fg, "_moves"))]
    colnames(rdb) <- c(paste0("P_", fg, "_moves"), paste0("O_", fg, "_moves"))
    rdb$Player_Col <- "Black"
    rrdb <- cbind(db, rdb)
  } else {
    rrdb <- data.frame()
  }

  # Return combined data if any games were found for the player
  if (nrow(db) + nrow(dw) > 0) {
    return(rbind(rrdw, rrdb))
  } else {
    message("No data found for this player name")
    return(data.frame())
  }
}
