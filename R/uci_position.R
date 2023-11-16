#' Send the 'position' command to a chess engine
#'
#' The UCI 'position' command sets up a chess position.
#'
#' @details The position command sets up a chess position from either a user
#'   supplied FEN string or series of moves in UCI long algebraic notation.
#'
#'   If the new position is from a different game than the last position,
#'   `uci_newgame()` should be called before calling `uci_position()`. For more
#'   details see the [UCI
#'   protocol](http://wbec-ridderkerk.nl/html/UCIProtocol.html).
#'
#' @param engine An engine handler created by [bigchess::uci_engine()].
#' @param startpos (default = `TRUE`) A Boolean indicating whether to use the
#'   starting position.
#' @param moves (default = NULL) A string representing chess moves in UCI long
#'   algebraic notation.
#' @param fen (default = NULL) A string representing a chess position in FEN
#'   format.
#'
#' @return An updated engine handler.
#'
#' @inherit uci_cmd seealso
#' @inherit uci_cmd examples
#'
#' @export

uci_position <- function(engine, moves = NULL, startpos = TRUE, fen = NULL) {
  # Set up the starting position, if needed
  if (startpos & is.null(fen)) {
    uci_cmd(engine, paste("position startpos moves", moves))
  }

  # Play the moves on the internal chess board
  if (!is.null(fen)) {
    uci_cmd(engine, paste("position fen", fen, "moves", moves))
  }

  # Return the updated engine object
  return(engine)
}
