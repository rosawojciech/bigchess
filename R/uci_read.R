#' Read the current stdout from a chess engine
#'
#' This function reads the current standard output (stdout) from a [UCI
#' compatible](http://wbec-ridderkerk.nl/html/UCIProtocol.html) chess engine.
#'
#' @details The function reads the current output from the chess engine and
#'   updates the engine handler's temp element with this information.
#'
#' @param engine An engine handler created by [bigchess::uci_engine()].
#'
#' @return An updated engine handler.
#'
#' @inherit uci_cmd seealso
#' @inherit uci_cmd examples
#'
#' @export

uci_read <- function(engine) {
  # Read current stdout from the chess engine
  prs <- engine$pipe$read_output_lines()

  # Update the engine object if new output is available
  if (length(prs) > 0) {
    engine$temp <- c(engine$temp, prs)
  }

  # Return the updated engine object
  return(engine)
}
