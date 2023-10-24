#' Send the 'ucinewgame' command to a chess engine
#'
#' This function sends the 'ucinewgame' command to a chess engine.
#'
#' @details The 'ucinewgame' command is used to inform the chess engine that the
#'   next search will be from a different game. For more details see the [UCI
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

uci_ucinewgame <- function(engine) {
  # Send 'ucinewgame' command to the chess engine
  return(uci_cmd(engine, "ucinewgame"))
}
