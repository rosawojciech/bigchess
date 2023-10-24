#' Send the 'setoption' command to a chess engine
#'
#' This function sends the 'setoption' command to a UCI compatible chess engine.
#'
#' @details The 'setoption' command is used to change the internal parameters of
#'   the chess engine. For more details see the [UCI
#'   protocol](http://wbec-ridderkerk.nl/html/UCIProtocol.html).
#'
#' @param engine An engine handler created by [bigchess::uci_engine()].
#' @param name A string giving the option name.
#' @param value A string giving the option value.
#'
#' @return An updated engine handler.
#'
#' @inherit uci_cmd seealso
#' @inherit uci_cmd examples
#'
#' @export

uci_setoption <- function(engine, name = NULL, value = NULL) {
  # Send 'setoption' command if name and value are provided
  if (!is.null(name) & !is.null(value)) {
    return(uci_cmd(engine, paste("setoption", name, "value", value)))
  }
}
