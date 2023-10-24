#' Send the 'stop' command to a chess engine
#'
#' This function sends the 'stop' command to a UCI compatible chess engine.
#'
#' @details The 'stop' command is used to halt the chess engine's calculations
#'   as soon as possible. For more details see the [UCI
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

uci_stop <- function(engine) {
  # Send 'stop' command to the chess engine
  return(uci_cmd(engine, "stop"))
}
