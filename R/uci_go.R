#' Sending command go for chess engine
#'
#' Sending command go for chess engine. Info about go command from http://wbec-ridderkerk.nl/html/UCIProtocol.html
#' start calculating on the current position set up with the "position" command. There are a number of commands that can follow this command, all will be sent in the same string. If one command is not send its value should be interpreted as it would not influence the search.
#'
#' @param engine engine object
#' @param depth integer depth (search x plies only)
#' @param infinite boolean default FALSE. If TRUE, stoptime (next argument) should be defined
#' @param stoptime integer default 1. Used in Sys.sleep after go infinite in egine. After this, uci_stop() is executed
#' @param wtime integer default NULL (white has x msec left on the clock)
#' @param btime integer default NULL (black has x msec left on the clock)
#' @param winc integer default NULL (white increment per move in mseconds if x > 0)
#' @param binc integer default NULL (black increment per move in mseconds if x > 0)
#' @return engine object
#'
#' @examples
#'\donttest{
#' # Linux (make sure you have executable permission):
#' engine_path <- "./stockfish_10_x64"
#' # Windows
#' # engine_path <- "./stockfish_10_x64.exe"
#' e <- uci_engine(engine_path)
#' e <- uci_go(depth = 10)
#' uci_quit(e)
#' # Using pipe '%>%' from magrittr:
#' require(magrittr)
#' uci_engine(engine_path) %>% uci_go(depth = 10) %>% uci_quit()
#' # Find best answer for black after 1. e4 in 100 seconds:
#' uci_engine(engine_path) %>% uci_position(moves = "e2e4") %>%
#'   uci_go(depth = 20) %>% uci_quit() %>% uci_parse()
#' # Find best answer for black after 1. e4 in 100 seconds:
#' uci_engine(engine_path) %>% uci_position(moves = "e2e4") %>%
#'   uci_go(infinite = TRUE,stoptime = 100) %>% uci_quit() %>% uci_parse()}
#' @export
uci_go <- function(engine,depth = NULL,infinite = FALSE, stoptime = 1, wtime = NULL, btime = NULL, winc = NULL, binc = NULL){
  if(!infinite){
    ccmd <- "go"
    if(!is.null(depth)) ccmd <- paste(ccmd,"depth",depth)
    if(!is.null(wtime)) ccmd <- paste(ccmd,"wtime",wtime)
    if(!is.null(btime)) ccmd <- paste(ccmd,"btime",btime)
    if(!is.null(winc)) ccmd <- paste(ccmd,"winc",winc)
    if(!is.null(binc)) ccmd <- paste(ccmd,"binc",binc)
    uci_cmd(engine,ccmd)
  }
  else{
    uci_cmd(engine,"go infinite")
    Sys.sleep(stoptime)
    uci_stop(engine)
  }
  if(is.null(depth)&is.null(wtime)&is.null(btime)&is.null(winc)&is.null(binc)){
    Sys.sleep(stoptime)
    uci_stop(engine)
  }
  rr <- ""
  while(length(grep("bestmove",rr))<1){
    engine <- uci_read(engine)
    rl <- engine$temp
    rr <- rl[length(rl)]
  }
  return(engine)
}
