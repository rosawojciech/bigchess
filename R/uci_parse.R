#' Parse GUI commands from chess engine
#'
#' Parse GUI commands from chess engine.
#'
#' @param ucilog strings from uci_quit() or uci_read()$temp
#' @param filter string default 'bestmove' (currently only this value is available)
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
  rslt <- ucilog[grepl(filter,ucilog)]
  if(filter == "bestmove"){
    rslt <- gsub("bestmove ","",rslt)
    return(gsub(" ponder [a-z0-9]+","",rslt))
  }
}
