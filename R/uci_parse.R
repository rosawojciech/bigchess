#' Parse GUI commands from chess engine
#'
#' Parse GUI commands from chess engine.
#'
#' @param ucilog strings from uci_quit() or uci_read()$temp
#' @param filter string, one of 'bestmove' (default), 'score' or 'bestline'
#' @return strings with parsed information from engine
#'
#' @examples
#'\donttest{
#' # Linux (make sure you have executable permission):
#' engine_path <- "./stockfish_10_x64"
#' # Windows
#' # engine_path <- "./stockfish_10_x64.exe"
#' e <- uci_engine(engine_path)
#' e <- uci_go(depth = 10)
#' rslt <- uci_quit(e)
#' uci_parse(rslt)
#' # Using pipe '%>%' from magrittr:
#' require(magrittr)
#' uci_engine(engine_path) %>% uci_go(depth = 10) %>% uci_quit() %>% uci_parse()}
#' @export
uci_parse <- function(ucilog,filter = "bestmove"){
  if(filter == "bestmove"){
    rslt <- ucilog[grepl(filter,ucilog)]
    rslt <- gsub("bestmove ","",rslt)
    up <- gsub(" ponder [a-z0-9]+","",rslt)
  }
  if(filter == "score"){
    m <- gregexpr("score cp((?:\\s|\\s\\-)[0-9]+)",ucilog)
    rslt <- as.integer(gsub("score cp ", " ",unlist(regmatches(ucilog,m))))
    up <- tail(rslt,n=1)
  }
  if(filter == "bestline"){
    m <- gregexpr(" pv [a-z0-9 ]+",ucilog)
    rslt <- gsub(" pv ", "",unlist(regmatches(ucilog,m)))
    up <- tail(rslt,n=1)
  }
  return(up)
}
