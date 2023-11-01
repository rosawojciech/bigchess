utils::globalVariables(c("LAN"))
#' Analyze a chess game
#'
#' This function analyzes a chess game using a
#' [UCI-compatible](https://wbec-ridderkerk.nl/html/UCIProtocol.html) chess
#' engine.
#'
#' @details The moves of the game may be provided in either short algebraic
#'   notation (SAN) or UCI long algebraic notation (LAN). Note that if it is
#'   black's turn, the score is multiplied by -1 so that a positive score always
#'   means white has the advantage, and negative always means black has the
#'   advantage.
#'
#' @inheritParams analyze_position
#' @param quiet (default = `FALSE`) A Boolean. Hide system messages?
#'
#' @return A list containing the results from `analyze_position()` for each
#'   position in the game.
#'
#' @seealso [bigchess::analyze_position()]
#'
#' @examples
#' # To run the example code, place a UCI-compatible chess engine in the
#' # `bigchess` subdirectory `/inst/extdata/engine`, or replace the line below
#' # with a valid path:
#' # engine_path <- "/put/your/own/engine/path/here"
#' engine_path <- find_engine()
#' if(!is.null(engine_path)) {
#'   san <- "1. e4 e5 2. Nf3 Nc6 3. d4 exd4 4. Bc4 Nf6 5. O-O Be7"
#'   game <- analyze_game(engine_path, san = san, depth = 2)
#'   game[[1]] # Analysis results for the first position
#' } else {
#'  message(
#'    paste0(
#'      'To run the examples, install a chess engine in /inst/extdata/',
#'      'engine,\n or replace engine_path with the path to an engine.'
#'    )
#'  )
#' }
#' @export
analyze_game <- function(engine, san = NULL, lan = NULL, quiet = FALSE, ...) {
  # Initialize the UCI engine
  if (is.character(engine))
    e <- uci_engine(path = engine)
  else
    e <- engine

  # Convert SAN to LAN notation if provided
  if (!is.null(san))
    lan <- san2lan(san)

  # Convert LAN to SAN notation if provided
  if (!is.null(lan))
    san <- lan2san(lan)

  P <- list()

  # Split LAN notation into individual moves
  guci <- lan
  gucil <- strsplit(guci, " ")[[1]]

  # Split SAN notation into individual moves
  gsan <- strsplit(gsub("[0-9]+\\. ", "", san), " ")[[1]]

  for (i in 1:length(gucil)) {
    t <- list()
    t$curmove_lan <- gucil[i]
    t$curmove_san <- gsan[i]
    t$curpos_lan <- paste0(gucil[1:i], collapse = " ")
    t$curpos_san <- lan2san(t$curpos_lan)

    # Check if the current position is in the opening book
    if (nrow(subset(bigchess::eco, LAN == t$curpos_lan)) == 0) {
      e <- uci_position(e, moves = t$curpos_lan)
      e <- uci_go(e, ...)
      t$ucilog <- uci_read(e)$temp
      t$score <- uci_parse(t$ucilog, "score")
      if (i %% 2) t$score <- -t$score
      t$bestmove <- uci_parse(t$ucilog)
      t$bestline_lan <- uci_parse(t$ucilog, "bestline")
      l2s <- lan2san(paste(t$curpos_lan, t$bestline_lan))
      t$bestline_san <- substr(l2s, nchar(t$curpos_san) + 2, nchar(l2s))
      t$bestmove_san <- strsplit(gsub("[0-9]+\\. ", "", t$bestline_san), " ")[[1]][1]
      t$comment <- ""
    } else {
      t$comment <- "book"
    }

    P[[i]] <- t
    if (!quiet) message(paste("Move", i, Sys.time()))
  }

  # Quit the UCI engine
  uci_quit(e)

  if (!quiet) message("Done!")
  return(P)
}
