#' Open a chess engine
#'
#' This function opens a chess engine and returns an "engine handler" consisting
#' of a named list containing a connection to the engine, and temporary storage
#' for the engine's output.
#'
#' @details The function `uci_engine()` establishes a connection to an external
#'   chess engine that uses the Universal Chess Interface [UCI
#'   protocol](https://wbec-ridderkerk.nl/html/UCIProtocol.html). Communication
#'   with the chess engine is handled using the
#'   [processx](https://github.com/r-lib/processx#readme) package.
#'
#'   The function returns a list that serves as an "engine handler". Commands
#'   and updates can be sent or received by passing the engine handler as an
#'   argument to other `bigchess` UCI functions. (See the examples below.) The
#'   engine can be closed by passing the engine handler as an argument to
#'   [bigchess::uci_quit()].
#'
#' @param path A string specifying the path to a chess engine. Ensure
#'   that you have executable permission on this file.
#'
#' @return An engine handler consisting of a list containing two named elements:
#'   * pipe - a connection to a chess engine
#'   * temp - temporary storage for the engine's output
#'
#' @inherit uci_cmd seealso
#' @inherit uci_cmd examples
#'
#' @export
uci_engine <- function(path){
  engine_handler <- list()
  engine_handler$pipe <- processx::process$new(command = path,
                                               stdout  = "|",
                                               stdin   = "|")
  engine_handler$temp <- character(0)
  return(uci_isready(engine_handler))
}
