#' Send a command to a chess engine
#'
#' Sends a universal chess interface (UCI) command to a
#' [UCI-compatible](https://wbec-ridderkerk.nl/html/UCIProtocol.html) chess
#' engine.
#'
#' @details For a list of available commands, see the [UCI
#'   protocol](https://wbec-ridderkerk.nl/html/UCIProtocol.html). For
#'   engine-specific UCI commands, refer to your engine's documentation. See
#'   [bigchess::uci_engine()] for details on engine objects.
#'
#' @param engine An engine handler created by [bigchess::uci_engine()].
#' @param command A string containing the command to be sent.
#'
#' @return An updated engine handler.
#'
#' @seealso
#' * [bigchess::uci_cmd()]
#' * [bigchess::uci_debug()]
#' * [bigchess::uci_engine()]
#' * [bigchess::uci_go()]
#' * [bigchess::uci_isready()]
#' * [bigchess::uci_parse()]
#' * [bigchess::uci_ponderhit()]
#' * [bigchess::uci_position()]
#' * [bigchess::uci_quit()]
#' * [bigchess::uci_read()]
#' * [bigchess::uci_register()]
#' * [bigchess::uci_setoption()]
#' * [bigchess::uci_stop()]
#' * [bigchess::uci_uci()]
#' * [bigchess::uci_ucinewgame()]
#'
#' @examples
#' # To run the example code, place a UCI-compatible chess engine in the
#' # bigchess subdirectory /inst/extdata/engine, or replace the line below with
#' # a path to a local UCI-compatible chess engine:
#' # engine_path <- "/put/your/own/engine/path/here"
#'
#' engine_path <- find_engine() # Checks to see if an engine is available
#' if (!is.null(engine_path)) { # Runs examples only if an engine is available
#'
#'   # Examples are shown for the entire family of UCI engine functions. Run the
#'   # uci_engine example first to create an engine for the other examples. Run
#'   # the uci_quit example last to shut down the engine when done.
#'
#'   # Create an engine handler
#'   engine <- uci_engine(engine_path)
#'
#'   # Set up a position using moves or FEN
#'   engine <- uci_position(engine, moves = 'e2e4 c7c5')
#'   engine <- uci_position(
#'     engine,
#'     fen = 'rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1'
#'   )
#'
#'   # Start calculating
#'   uci_go(engine, depth = 2)
#'
#'   # Read and parse engine output
#'   result <- uci_go(engine, depth = 2) |> uci_read()
#'   ucilog <- uci_parse(result$temp)
#'
#'   # Send a custom command
#'   engine <- uci_cmd(engine, "go depth 2")
#'
#'   # Stop calculations
#'   engine <- uci_cmd(engine, "go depth 1000") |> uci_stop()
#'
#'   # Change the engine's internal parameters
#'   engine <- uci_setoption(engine, name  = "Clear", value = "Hash")
#'
#'   # Turn debug mode on or off
#'   engine <- uci_debug(engine, TRUE)
#'   engine <- uci_debug(engine, FALSE)
#'
#'   # Register the engine
#'   engine <- uci_register(engine, later = TRUE)
#'   engine <- uci_register(engine, name = 'name', code = 'code')
#'
#'   # Send various other UCI commands to the engine
#'   engine <- uci_uci(engine)
#'   engine <- uci_ucinewgame(engine)
#'   engine <- uci_isready(engine)
#'   engine <- uci_ponderhit(engine)
#'
#'   # Shut down the engine
#'   uci_quit(engine)
#'
#' } else {
#'  stop(paste0('To run the examples, install a chess engine in /inst/extdata/',
#'              'engine,\n or replace engine_path with the path to an engine.'))
#' }
#' @export

uci_cmd <- function(engine, command = ""){
  engine$pipe$write_input(paste0(command, "\n"))
  return(engine)
}
