#' Send the 'uci' command to a chess engine
#'
#' This function sends the 'uci' command to a chess engine.
#'
#' @details The 'uci' command is used to switch the chess engine to UCI mode.
#'   For more details see the [UCI
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

uci_uci <- function(engine) {
  # Send 'uci' command to the chess engine
  return(uci_cmd(engine, "uci"))
}
