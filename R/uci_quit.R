#' Send the 'quit' command to a chess engine
#'
#' This function sends the 'quit' command to a chess engine.
#'
#' @details The 'quit' command is used to close the chess engine and to capture
#'   the engine's final output. For more details see the [UCI
#'   protocol](http://wbec-ridderkerk.nl/html/UCIProtocol.html).
#'
#' @param engine An engine handler created by [bigchess::uci_engine()].
#'
#' @return An updated engine handler.
#'
#' @inherit uci_cmd seealso
#' @inherit uci_cmd examples
#'
#' @export

uci_quit <- function(engine) {
  # Send 'quit' command to the chess engine
  uci_cmd(engine, "quit")

  # Update the engine and clean up
  rslt <- uci_read(engine)$temp
  engine$pipe$kill()

  # Return strings from UCI chess engine GUI
  return(rslt)
}
