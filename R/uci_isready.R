#' Check if a chess engine is ready
#'
#' This function sends the UCI 'isready' command to a chess engine.
#'
#' @details The 'isready' command is used to synchronize the engine with the
#'   GUI. When the GUI has sent commands that take time to complete,
#'   `uci_isready()` can be used to wait for the engine to be ready again or to
#'   ping the engine to find out if it is still alive. See the [UCI
#'   protocol](http://wbec-ridderkerk.nl/html/UCIProtocol.html) for more
#'   details.
#'
#' @param engine An engine handler created by [bigchess::uci_engine()].
#'
#' @return An updated engine handler.
#'
#' @inherit uci_cmd seealso
#' @inherit uci_cmd examples
#'
#' @export
uci_isready <- function(engine) {
  # Send 'isready' command to the engine
  uci_cmd(engine, "isready")

  # Loop until 'readyok' is received from the engine
  isr <- ""
  while (isr != "readyok") {
    # Read the latest output from the engine
    engine <- uci_read(engine)
    tisr <- tail(engine$temp, n = 1)

    # If the last message is valid, update isr
    if (length(tisr) > 0 && !is.na(tisr) && !is.null(tisr)) {
      isr <- tisr
    }
  }

  # Return the updated engine object
  return(engine)
}
