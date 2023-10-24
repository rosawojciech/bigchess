#' Send the 'debug' command to a chess engine
#'
#' Turns the UCI debug mode on or off.
#'
#' @details In debug mode the engine sends additional information to the GUI to
#'   help with debugging. This mode is off by default. The command may be sent
#'   any time, even when the engine is thinking. For more details see the [UCI
#'   Protocol](http://wbec-ridderkerk.nl/html/UCIProtocol.html).
#'
#' @param engine An engine handler created by [bigchess::uci_engine()].
#' @param on Boolean default TRUE
#'
#' @return An updated engine handler
#'
#' @inherit uci_cmd examples
#' @inherit uci_cmd seealso
#'
#' @export
uci_debug <- function(engine, on = TRUE){
  if(on) return(uci_cmd(engine, "debug on"))
  else   return(uci_cmd(engine, "debug off"))
}
