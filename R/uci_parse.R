#' Parse GUI commands from a chess engine
#'
#' This function parses results that were returned from a [UCI
#' protocol](https://wbec-ridderkerk.nl/html/UCIProtocol.html) chess engine.
#'
#' @details  The function takes output from `uci_quit()` or `uci_read()$temp`
#'   and returns the specified output. The type of output ('bestmove', 'score',
#'   or 'bestline') is specified using the `filter` parameter.
#'
#' @param ucilog A character vector of output from `uci_quit()` or
#'   `uci_read()$temp`.
#' @param filter (default = 'bestmove') A string representing one of 'bestmove',
#'   'score', or 'bestline'.
#'
#' @return A character vector of the results specified by `filter`.
#'
#' @inherit uci_cmd seealso
#' @inherit uci_cmd examples
#'
#' @export
#' @export
uci_parse <- function(ucilog, filter = "bestmove") {
  # Extraxt 'bestmove'
  if (filter == "bestmove") {
    rslt <- ucilog[grepl(filter, ucilog)]
    rslt <- gsub("bestmove ", "", rslt)
    up <- gsub(" ponder [a-z0-9]+", "", rslt)
  }

  # Extract 'score'
  if (filter == "score") {
    m <- gregexpr("score cp((?:\\s|\\s\\-)[0-9]+)", ucilog)
    rslt <- as.integer(gsub("score cp ", " ", unlist(regmatches(ucilog, m))))
    up <- tail(rslt, n = 1)
  }

  # Extract 'bestline'
  if (filter == "bestline") {
    m <- gregexpr(" pv [a-z0-9 ]+", ucilog)
    rslt <- gsub(" pv ", "", unlist(regmatches(ucilog, m)))
    up <- tail(rslt, n = 1)
  }

  # Return the result
  return(up)
}

