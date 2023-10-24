#' Count the number of times each type of piece moved
#'
#' The function creates a data frame containing the number of times each type of
#' piece was moved.
#'
#' @details The function counts the number of moves for each type of chess piece
#'   (pawn, knight, bishop, rook, queen, and king) for both white and black
#'   players, and calculates the total number of moves for each side (white,
#'   black), as well as the combined total. The results are returned in a data
#'   frame. The `movetext` argument must use The standard English piece
#'   abbreviations: pawn = "P" (optional; often omitted), knight = "N", bishop =
#'   "B", rook = "R", queen = "Q", and king = "K".
#'
#' @param movetext A character vector of movetext from a PGN.
#' @param sides (default = "both") A string indicating which side's moves to
#'   count ("white", "black", or "both").
#'
#' @return A data frame containing move count statistics for white and black, as
#'   well as the total.
#'
#' @examples
#' stat_moves("1. e4 e5 2. Nf3 Nf5 3. d5 ")
#'
#' @export
stat_moves <- function(movetext, sides = "both") {
  w <- data.frame()  # Initialize an empty data frame to store the results

  # Count moves for white
  if (sides %in% c("both", "white")) {
    w <- stat_moves_white(movetext)
  } else {
    w <- stat_moves_white(movetext)
  }

  # Count moves for black
  if (sides %in% c("both", "black")) {
    if (sides == "black") {
      w <- stat_moves_black(movetext)
    } else {
      w <- cbind(w, stat_moves_black(movetext))
    }
  }

  # If sides = "both" combine the stats
  if (sides %in% c("both")) {
    for (i in c("B", "K", "N", "O", "Q", "R")) {
      # Create new columns in the data frame with combined move statistics
      w[, paste0(i, "_moves")] <- w[, paste0("W_", i, "_moves")] +
                                  w[, paste0("B_", i, "_moves")]
    }
  }

  return(w)  # Return the data frame with move statistics
}

#' Count the number of times each type of white piece was moved
#'
#' This function generates a data frame that records the number of times each
#' type of white chess piece has moved.
#'
#' @details The function tallies the moves for each type of white chess piece
#'   (Bishop, King, Knight, Queen, Rook). The input moves should be a string of
#'   chess moves.
#'
#' @param moves A string representing a sequence of chess moves.
#'
#' @return A data frame containing the counts of moves for each type of white
#'   chess piece.
#'
#' @examples
#' moves <- "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4"
#' white_move_counts <- stat_moves_white(moves)
#'
#' @export
stat_moves_white <- function(moves) {
  # Remove move numbers from the string
  x <- gsub("([0-9+]{0,}\\.)", "", moves, perl = TRUE)

  # Replace double spaces with single spaces and trim leading/trailing spaces
  x <- trimws(gsub("  ", " ", x))

  # Split the string into individual moves
  sx <- strsplit(x, " ")

  # Initialize a data frame to store the results
  r <- data.frame(t(sapply(sx, function(i) {
    # Get the length of the vector i
    li <- length(i)

    if (li > 0) {
      # Count the number of times each piece has moved and return as a vector
      return(as.vector(unlist(table(substr(i[seq(1, li, by = 2)], 1, 1))[c("B", "K", "N", "O", "Q", "R")])))
    } else {
      # If there are no moves, return a vector of zeros
      return(rep(0, times = 6))
    }
  })), row.names = NULL)

  # Replace NA values with zeros
  r[is.na(r)] <- 0

  # Rename columns to indicate which piece's moves are being counted
  colnames(r) <- paste0(c("B", "K", "N", "O", "Q", "R"), "_moves")

  # Combine rook and king move counts (for castling)
  r$R_moves <- r$R_moves + r$O_moves
  r$K_moves <- r$K_moves + r$O_moves

  # Add a prefix to column names to indicate these are white pieces' move counts
  colnames(r) <- paste0("W_", colnames(r))

  return(r)
}

#' Count the number of times each type of black piece was moved
#'
#' This function takes a sequence of chess moves and calculates the number of
#' moves for each type of black chess piece (Bishop, King, Knight, Queen, Rook).
#' The input moves should be a string of chess moves.
#
#' @param moves A string representing a sequence of chess moves.
#'
#' @return A data frame containing the counts of moves for each type of black
#' chess piece.
#'
#' @examples
#' moves <- "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4"
#' black_move_counts <- stat_moves_black(moves)
#'
#' @export
stat_moves_black <- function(moves) {
  # Remove move numbers from the string
  x <- gsub("([0-9+]{0,}\\.)", "", moves, perl = TRUE)

  # Replace double spaces with single spaces and trim leading/trailing spaces
  x <- trimws(gsub("  ", " ", x))

  # Split the string into individual moves
  sx <- strsplit(x, " ")

  # Initialize a data frame to store the results
  r <- data.frame(t(sapply(sx, function(i) {
    # Get the length of the vector i
    li <- length(i)

    if (li > 1) {
      sli <- seq(2, li, by = 2)
    } else {
      sli <- 0
    }

    if (li > 1) {
      # Count the number of times each piece has moved and return as a vector
      return(as.vector(unlist(table(substr(i[sli], 1, 1))[c("B", "K", "N", "O", "Q", "R")])))
    } else {
      # If there are no moves, return a vector of zeros
      return(rep(0, times = 6))
    }
  })), row.names = NULL)

  # Replace NA values with zeros
  r[is.na(r)] <- 0

  # Rename columns to indicate which piece's moves are being counted
  colnames(r) <- paste0(c("B", "K", "N", "O", "Q", "R"), "_moves")

  # Combine rook and king move counts (for castling)
  r$R_moves <- r$R_moves + r$O_moves
  r$K_moves <- r$K_moves + r$O_moves

  # Add a prefix to column names to indicate these are black pieces' move counts
  colnames(r) <- paste0("B_", colnames(r))

  return(r)
}
