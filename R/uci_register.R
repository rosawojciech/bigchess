#' Send the 'register' command to a chess engine
#'
#' This function sends the 'register' command to a chess engine.
#'
#' @details The 'register' command is for engines that require user
#'   registration. For details see the [UCI
#'   protocol](http://wbec-ridderkerk.nl/html/UCIProtocol.html).
#'
#' @param engine An engine handler created by [bigchess::uci_engine()].
#' @param later (default = `TRUE`) A Boolean indicating whether to register
#'   later.
#' @param name (default = `NULL`) A string giving the registration name.
#' @param code (default = `NULL`) A string giving the registration code.
#'
#' @return An updated engine handler.
#'
#' @inherit uci_cmd seealso
#' @inherit uci_cmd examples
#'
#' @export

uci_register <- function(engine, later = TRUE, name = NULL, code = NULL) {
  # Send 'register later'
  if (later) {
    uci_cmd(engine, "register later")
  }

  # Send name
  if (!is.null(name)) {
    uci_cmd(engine, paste("register name", name))
  }

  # Send code
  if (!is.null(code)) {
    uci_cmd(engine, paste("register code", code))
  }

  # Return the updated engine object
  return(engine)
}
