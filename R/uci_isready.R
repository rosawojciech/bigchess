#' Checking if chess engine is ready
#'
#' Checking if chess engine is ready - sending command isready and parsing GUI until readyok is obtained. Info about isready command from http://wbec-ridderkerk.nl/html/UCIProtocol.html
#' This is used to synchronize the engine with the GUI. When the GUI has sent a command or multiple commands that can take some time to complete, this command can be used to wait for the engine to be ready again or to ping the engine to find out if it is still alive. E.g. this should be sent after setting the path to the tablebases as this can take some time. This command is also required once before the engine is asked to do any search to wait for the engine to finish initializing. This command must always be answered with "readyok" and can be sent also when the engine is calculating in which case the engine should also immediately answer with "readyok" without stopping the search.
#'
#' @param engine engine object
#' @return engine object
#'
#' @examples
#'\donttest{
#' # Linux (make sure you have executable permission):
#' engine_path <- "./stockfish_10_x64"
#' # Windows
#' # engine_path <- "./stockfish_10_x64.exe"
#' e <- uci_engine(engine_path)
#' e <- uci_isready(e)
#' uci_quit(e)
#' # Using pipe '%>%' from magrittr:
#' require(magrittr)
#' uci_engine(engine_path) %>% uci_isready() %>% uci_quit()}
#' @export
uci_isready <- function(engine){
  uci_cmd(engine,"isready")
  isr <- ""
  while(isr!="readyok") {
    engine <- uci_read(engine)
    tisr <- tail(engine$temp,n = 1)
    if(length(tisr)>0) if(!is.na(tisr) & !is.null(tisr)) isr <- tisr
  }
  return(engine)
}
