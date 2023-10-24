#' Send the 'ponderhit' command to a chess engine
#'
#' Inform the engine that its predicted move was correct.
#'
#' @details The 'ponderhit' command is used when the move that the engine was
#'   pondering is actually played by the opponent. After receiving this command,
#'   the engine should execute the move it was pondering. For more details see
#'   the [UCI protocol](http://wbec-ridderkerk.nl/html/UCIProtocol.html).
#'
#' @param engine An engine handler created by [bigchess::uci_engine()].
#'
#' @return An updated engine handler.
#'
#' @inherit uci_cmd seealso
#' @inherit uci_cmd examples
#'
#' @export
uci_ponderhit <- function(engine){
  return(uci_cmd(engine, "ponderhit"))
}
