#' Analyze a chess position
#'
#' This function analyzes a chess position using a
#' [UCI-compatible](https://wbec-ridderkerk.nl/html/UCIProtocol.html) chess
#' engine.
#'
#' @details The function analyzes a chess position represented by a series of
#'   chess moves in either short algebraic notation (SAN) or UCI long algebraic
#'   notation (LAN), and uses a UCI-compatible chess engine to analyze the
#'   position. The results include the best move, score, and best lines.
#'
#' @param engine Either a character string giving the path to a chess engine, or
#'   an engine handler created by [bigchess::uci_engine()].
#' @param san (default = `NULL`) A character string giving a series of chess
#'   moves in short algebraic notation.
#' @param lan (default = `NULL`) A character string giving a series of chess
#'   moves in UCI long algebraic notation.
#' @param ... Further arguments passed to [bigchess::uci_go()].
#'
#' @return A list containing the results of the analysis.
#'
#' @seealso [bigchess::analyze_game()]
#'
#' @examples
#' # To run the example code, place a UCI-compatible chess engine in the
#' # `bigchess` subdirectory `/inst/extdata/engine`, or replace the line below
#' # with a valid path:
#' # engine_path <- "/put/your/own/engine/path/here"
#' engine_path <- find_engine()
#' if (!is.null(engine_path)) {
#'   analyze_position(engine_path, san = "1. e4", depth = 2)
#' } else {
#'  message(
#'    paste0(
#'      'To run the examples, install a chess engine in /inst/extdata/',
#'      'engine,\n or replace engine_path with the path to an engine.'
#'    )
#'  )
#' }
#'
#' @export

analyze_position <- function(engine, san = NULL, lan = NULL, ...) {

  # Create an engine handler
  if (is.character(engine)) {
    e <- uci_engine(path = engine)
  } else {
    e <- engine
  }

  # Do the LAN/SAN conversions
  if (!is.null(san)) {
    lan <- san2lan(san)
  }
  if (!is.null(lan)) {
    san <- lan2san(lan)
  }

  # Set up the position and start the analysis
  e <- uci_position(e, moves = lan)
  e <- uci_go(e, ...)

  # Read the output from the chess engine
  ucilog <- uci_quit(e)

  # Parse and store the output
  r <- list()
  r$bestmove_lan <- uci_parse(ucilog)
  r$score <- uci_parse(ucilog, "score")
  r$bestline_lan <- uci_parse(ucilog, "bestline")
  r$curpos_lan <- lan
  r$curpos_san <- san

  # Convert LAN to SAN for bestline and bestmove
  l2s <- lan2san(paste(r$curpos_lan, r$bestline_lan))
  r$bestline_san <- substr(l2s, nchar(r$curpos_san) + 2, nchar(l2s))
  r$bestmove_san <- strsplit(gsub("[0-9]+\\. ", "", r$bestline_san), " ")[[1]][1]

  # Add a comment field
  r$comment <- ""

  # Return the result
  return(r)
}
