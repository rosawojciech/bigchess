#' Send the 'go' command to a chess engine
#'
#' This function sends the UCI 'go' command to a chess engine.
#'
#' @details The 'go' command starts calculation at the current position. For
#'   more details see the [UCI
#'   protocol](https://wbec-ridderkerk.nl/html/UCIProtocol.html).
#'
#' @param engine An engine handler created by [bigchess::uci_engine()].
#' @param depth (default = `NULL`) An integer stating the desired search depth
#'   in ply.
#' @param infinite (default = `FALSE`) A Boolean indicating if the search should
#'   be allowed to run indefinitely. If `TRUE`, `stoptime` should be defined.
#' @param stoptime (default = 1) An integer giving the maximum search time in
#'   seconds when `infinite` = `TRUE`.
#' @param wtime (default = `NULL`) An integer stating white's remaining time in
#'   milliseconds.
#' @param btime (default = `NULL`) An integer stating black's remaining time in
#'   milliseconds.
#' @param winc (default = `NULL`) An integer stating white's increment per move
#'   in milliseconds.
#' @param binc (default = `NULL`) An integer stating black's increment per move
#'   in milliseconds.
#'
#' @return An updated engine handler.
#'
#' @inherit uci_cmd seealso
#' @inherit uci_cmd examples
#'
#' @export
uci_go <- function(engine, depth = NULL, infinite = FALSE, stoptime = 1,
                   wtime = NULL, btime = NULL, winc = NULL, binc = NULL) {
  # If the search is not infinite, construct and send the 'go' command with the
  # given parameters
  if (!infinite) {
    ccmd <- "go"
    if (!is.null(depth)) ccmd <- paste(ccmd, "depth", depth)
    if (!is.null(wtime)) ccmd <- paste(ccmd, "wtime", wtime)
    if (!is.null(btime)) ccmd <- paste(ccmd, "btime", btime)
    if (!is.null(winc))  ccmd <- paste(ccmd, "winc", winc)
    if (!is.null(binc))  ccmd <- paste(ccmd, "binc", binc)
    uci_cmd(engine, ccmd)
  } else {
    # If the search is infinite, send 'go infinite' command and then stop after
    # stoptime seconds
    uci_cmd(engine, "go infinite")
    Sys.sleep(stoptime)
    uci_stop(engine)
  }

  # If all parameters are NULL and search is not infinite, let the engine go for
  # stoptime seconds and then stop it
  if (is.null(depth) & is.null(wtime) & is.null(btime)
      & is.null(winc) & is.null(binc)) {
    Sys.sleep(stoptime)
    uci_stop(engine)
  }

  # Keep reading from the engine until a 'bestmove' line is found
  rr <- ""
  while (length(grep("bestmove", rr)) < 1) {
    engine <- uci_read(engine)
    rl <- engine$temp
    rr <- rl[length(rl)]
  }

  # Return the updated engine object
  return(engine)
}
