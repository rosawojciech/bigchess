#' Sending command position for chess engine
#'
#' Sending command position for chess engine. Info about position command from http://wbec-ridderkerk.nl/html/UCIProtocol.html
#' set up the position described in fenstring on the internal board and play the moves on the internal chess board. if the game was played  from the start position the string "startpos" will be sent Note: no "new" command is needed. However, if this position is from a different game than the last position sent to the engine, the GUI should have sent a "ucinewgame" inbetween.
#' @param engine engine object
#' @param startpos boolean default TRUE
#' @param moves string in long algebraic notation
#' @param fen string
#' @return engine object
#' @examples
#'\donttest{
#' # Linux (make sure you have executable permission):
#' engine_path <- "./stockfish_10_x64"
#' # Windows
#' # engine_path <- "./stockfish_10_x64.exe"
#' e <- uci_engine(engine_path)
#' e <- uci_position(e,moves = "e2e4")
#' e <- uci_go(e,depth = 10)
#' uci_quit(e)
#' # Using pipe '%>%' from magrittr:
#' require(magrittr)
#' uci_engine(engine_path) %>% uci_position(moves = "e2e4") %>%
#'   uci_go(depth = 10) %>% uci_quit() %>% uci_parse()}
#' @export
uci_position <- function(engine,moves = NULL,startpos = TRUE,fen = NULL ){
  if(startpos & is.null(fen)) uci_cmd(engine,paste("position startpos moves",moves))
  if(!is.null(fen)) uci_cmd(engine,paste("position fen",fen,"moves",moves))
  return(engine)
}
